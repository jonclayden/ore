#include <string.h>

#include <R.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <R_ext/Riconv.h>
#include <R_ext/Connections.h>

#include "text.h"

// If R is recent enough for R_GetConnection() to be available, and the connections API is the expected version, support reading from connections
#if !defined(DISABLE_CONNECTIONS) && defined(R_VERSION) && R_VERSION >= R_Version(3,3,0) && defined(R_CONNECTIONS_VERSION) && R_CONNECTIONS_VERSION == 1
#define USING_CONNECTIONS
#endif

// Initial buffer size when reading from a file; scales exponentially
#define FILE_BUFFER_SIZE    1024

// Case-insensitive comparison of (at most) the first "num" characters of two strings, considering only ASCII letters
// NB: tolower() is not used because its behaviour depends on the locale
int ore_strnicmp (const char *str1, const char *str2, size_t num)
{
    for (size_t i=0; i<num; i++)
    {
        const int c1 = (str1[i] >= 'A' && str1[i] <= 'Z') ? str1[i] - 'A' + 'a' : str1[i];
        const int c2 = (str2[i] >= 'A' && str2[i] <= 'Z') ? str2[i] - 'A' + 'a' : str2[i];
        if (c1 != c2)
            return c1 - c2;
        else if (c1 == '\0')
            break;
    }
    
    return 0;
}

// Step forward "n" characters from "p", returning NULL if the text ends first
UChar * ore_step (OnigEncoding enc, const UChar *p, const UChar *end, size_t n)
{
    while (n > 0 && p < end)
    {
        p += ONIGENC_MBC_ENC_LEN(enc, p, end);
        n--;
    }
    
    return (n == 0) ? (UChar *) p : NULL;
}

// Extend a vector to hold more values
// NB: This function is less efficient than standard C realloc(), because it always results in a copy, but using R_alloc simplifies things. The R API function S_realloc() is closely related, but seems to exist only "for compatibility with older versions of S", and zeroes out the extra memory, which is unnecessary here.
char * ore_realloc (const void *ptr, const size_t new_len, const size_t old_len, const int element_size)
{
    if (ptr == NULL)
        return (char *) R_alloc(new_len, element_size);
    else if (new_len <= old_len)
        return (char *) ptr;
    else
    {
        char *new_ptr;
        const size_t old_byte_len = old_len * element_size;
        
        new_ptr = R_alloc(new_len, element_size);
        memcpy(new_ptr, (const char *) ptr, old_byte_len);
        return new_ptr;
    }
}

// Known encoding names, in a normalised form (upper case, without separators), with their Oniguruma equivalents
typedef struct {
    const char    * name;
    OnigEncoding    onig_enc;
} encoding_alias_t;

static const encoding_alias_t encoding_aliases[] = {
    { "ASCII",          ONIG_ENCODING_ASCII },
    { "USASCII",        ONIG_ENCODING_ASCII },
    { "UTF8",           ONIG_ENCODING_UTF8 },
    { "ISO88591",       ONIG_ENCODING_ISO_8859_1 },
    { "LATIN1",         ONIG_ENCODING_ISO_8859_1 },
    { "ISO88592",       ONIG_ENCODING_ISO_8859_2 },
    { "LATIN2",         ONIG_ENCODING_ISO_8859_2 },
    { "ISO88593",       ONIG_ENCODING_ISO_8859_3 },
    { "LATIN3",         ONIG_ENCODING_ISO_8859_3 },
    { "ISO88594",       ONIG_ENCODING_ISO_8859_4 },
    { "LATIN4",         ONIG_ENCODING_ISO_8859_4 },
    { "ISO88595",       ONIG_ENCODING_ISO_8859_5 },
    { "ISO88596",       ONIG_ENCODING_ISO_8859_6 },
    { "ISO88597",       ONIG_ENCODING_ISO_8859_7 },
    { "ISO88598",       ONIG_ENCODING_ISO_8859_8 },
    { "ISO88599",       ONIG_ENCODING_ISO_8859_9 },
    { "LATIN5",         ONIG_ENCODING_ISO_8859_9 },
    { "ISO885910",      ONIG_ENCODING_ISO_8859_10 },
    { "LATIN6",         ONIG_ENCODING_ISO_8859_10 },
    { "ISO885911",      ONIG_ENCODING_ISO_8859_11 },
    { "ISO885913",      ONIG_ENCODING_ISO_8859_13 },
    { "LATIN7",         ONIG_ENCODING_ISO_8859_13 },
    { "ISO885914",      ONIG_ENCODING_ISO_8859_14 },
    { "LATIN8",         ONIG_ENCODING_ISO_8859_14 },
    { "ISO885915",      ONIG_ENCODING_ISO_8859_15 },
    { "LATIN9",         ONIG_ENCODING_ISO_8859_15 },
    { "ISO885916",      ONIG_ENCODING_ISO_8859_16 },
    { "LATIN10",        ONIG_ENCODING_ISO_8859_16 },
    { "UTF16BE",        ONIG_ENCODING_UTF16_BE },
    { "UTF16LE",        ONIG_ENCODING_UTF16_LE },
    { "UTF32BE",        ONIG_ENCODING_UTF32_BE },
    { "UTF32LE",        ONIG_ENCODING_UTF32_LE },
    { "BIG5",           ONIG_ENCODING_BIG5 },
    { "BIGFIVE",        ONIG_ENCODING_BIG5 },
    { "CP932",          ONIG_ENCODING_CP932 },
    { "WINDOWS31J",     ONIG_ENCODING_CP932 },
    { "CP1250",         ONIG_ENCODING_WINDOWS_1250 },
    { "WINDOWS1250",    ONIG_ENCODING_WINDOWS_1250 },
    { "CP1251",         ONIG_ENCODING_WINDOWS_1251 },
    { "WINDOWS1251",    ONIG_ENCODING_WINDOWS_1251 },
    { "CP1252",         ONIG_ENCODING_WINDOWS_1252 },
    { "WINDOWS1252",    ONIG_ENCODING_WINDOWS_1252 },
    { "CP1253",         ONIG_ENCODING_WINDOWS_1253 },
    { "WINDOWS1253",    ONIG_ENCODING_WINDOWS_1253 },
    { "CP1254",         ONIG_ENCODING_WINDOWS_1254 },
    { "WINDOWS1254",    ONIG_ENCODING_WINDOWS_1254 },
    { "CP1257",         ONIG_ENCODING_WINDOWS_1257 },
    { "WINDOWS1257",    ONIG_ENCODING_WINDOWS_1257 },
    { "EUCJP",          ONIG_ENCODING_EUC_JP },
    { "EUCKR",          ONIG_ENCODING_EUC_KR },
    { "EUCTW",          ONIG_ENCODING_EUC_TW },
    { "GB18030",        ONIG_ENCODING_GB18030 },
    { "KOI8R",          ONIG_ENCODING_KOI8_R },
    { "KOI8U",          ONIG_ENCODING_KOI8_U },
    { "SHIFTJIS",       ONIG_ENCODING_SJIS },
    { "SJIS",           ONIG_ENCODING_SJIS },
    { NULL,             NULL }
};

// Look up an encoding name, returning NULL if it isn't known
// Names are matched in full, ignoring case and the separators '-', '_' and ' ', so "ISO-8859-15" and "iso8859_15" are equivalent
static OnigEncoding ore_lookup_onig_enc (const char *enc)
{
    if (ore_strnicmp(enc, "native.enc", 11) == 0)
    {
        // The "ore.encoding" option gives the native encoding; if it is unset, or itself refers to the native encoding, we fall back to ASCII
        SEXP native_encoding = GetOption1(install("ore.encoding"));
        if (!isString(native_encoding) || length(native_encoding) < 1 || ore_strnicmp(CHAR(STRING_ELT(native_encoding,0)), "native.enc", 11) == 0)
            return ONIG_ENCODING_ASCII;
        else
        {
            OnigEncoding onig_enc = ore_lookup_onig_enc(CHAR(STRING_ELT(native_encoding, 0)));
            return (onig_enc == NULL) ? ONIG_ENCODING_ASCII : onig_enc;
        }
    }
    
    // Normalise the name; anything too long to fit cannot be a known encoding
    char normalised[ORE_ENCODING_NAME_MAX_LEN];
    size_t len = 0;
    const char *ptr;
    for (ptr = enc; *ptr != '\0' && len < ORE_ENCODING_NAME_MAX_LEN - 1; ptr++)
    {
        if (*ptr == '-' || *ptr == '_' || *ptr == ' ')
            continue;
        normalised[len++] = (*ptr >= 'a' && *ptr <= 'z') ? *ptr - 'a' + 'A' : *ptr;
    }
    normalised[len] = '\0';
    
    if (*ptr == '\0')
    {
        for (const encoding_alias_t *alias = encoding_aliases; alias->name != NULL; alias++)
        {
            if (strcmp(normalised, alias->name) == 0)
                return alias->onig_enc;
        }
    }
    
    return NULL;
}

// Convert an encoding string to its Oniguruma equivalent, falling back to ASCII (with a warning) if it isn't known
static OnigEncoding ore_name_to_onig_enc (const char *enc)
{
    OnigEncoding onig_enc = ore_lookup_onig_enc(enc);
    if (onig_enc == NULL)
    {
        warning("Encoding \"%s\" is not supported by Oniguruma - using ASCII", enc);
        onig_enc = ONIG_ENCODING_ASCII;
    }
    
    return onig_enc;
}

// Check whether a string consists entirely of ASCII characters
static Rboolean ore_is_ascii (const char *string)
{
    for (const unsigned char *ptr = (const unsigned char *) string; *ptr != '\0'; ptr++)
    {
        if (*ptr > 0x7f)
            return FALSE;
    }
    
    return TRUE;
}

// Create a consistent encoding structure from an existing type, propagating as closely as possible
encoding_t * ore_encoding (const char *name, OnigEncoding onig_enc, cetype_t *r_enc)
{
    // The fallback R encoding, where nothing else is marked
    cetype_t final_r_enc = CE_NATIVE;
    Rboolean convert = FALSE;
    
    // If there's no Oniguruma encoding, work from a name, if available
    const Rboolean have_name = (name != NULL && strlen(name) > 0);
    if (have_name && onig_enc == NULL)
        onig_enc = ore_name_to_onig_enc(name);
    
    // If there's no R encoding, take it from the Oniguruma one
    // R can only mark strings as UTF-8 or Latin-1 (or native), so text in any other named encoding is converted to UTF-8 when it is returned to R
    if (r_enc == NULL)
    {
        if (onig_enc == ONIG_ENCODING_UTF8)
            final_r_enc = CE_UTF8;
        else if (onig_enc == ONIG_ENCODING_ISO_8859_1)
            final_r_enc = CE_LATIN1;
        else if (have_name && onig_enc != ONIG_ENCODING_ASCII && ore_strnicmp(name, "native.enc", 11) != 0)
        {
            final_r_enc = CE_UTF8;
            convert = TRUE;
        }
        else
            final_r_enc = CE_NATIVE;
    }
    
    // Propagate back from the R encoding if necessary, but R asserts very few encodings
    // Strings in the native encoding are taken to be in the encoding given by the "ore.encoding" option
    if (r_enc != NULL)
    {
        final_r_enc = *r_enc;
        if (onig_enc == NULL)
        {
            switch (*r_enc)
            {
                case CE_UTF8:   onig_enc = ONIG_ENCODING_UTF8;                  break;
                case CE_LATIN1: onig_enc = ONIG_ENCODING_ISO_8859_1;            break;
                case CE_NATIVE: onig_enc = ore_lookup_onig_enc("native.enc");   break;
                default:        onig_enc = ONIG_ENCODING_ASCII;                 break;
            }
        }
    }
    
    // Create, populate and return the encoding structure
    encoding_t *encoding = (encoding_t *) R_alloc(1, sizeof(encoding_t));
    if (name != NULL)
    {
        strncpy(encoding->name, name, ORE_ENCODING_NAME_MAX_LEN-1);
        encoding->name[ORE_ENCODING_NAME_MAX_LEN-1] = '\0';
    }
    else
        encoding->name[0] = '\0';
    encoding->onig_enc = onig_enc;
    encoding->r_enc = final_r_enc;
    encoding->convert = convert;
    encoding->assumed = FALSE;
    
    return encoding;
}

// Create an encoding structure for an R string (CHARSXP), based on its declared encoding and content
// Strings containing only ASCII characters are treated as ASCII, since they are valid in any ASCII-compatible encoding
// Other strings without a declared encoding are assumed to be in the native encoding (given by the "ore.encoding" option), but may be in another encoding if the regex says so
encoding_t * ore_string_encoding (SEXP string)
{
    cetype_t r_enc = getCharCE(string);
    if (r_enc == CE_BYTES || ore_is_ascii(CHAR(string)))
        return ore_encoding(NULL, ONIG_ENCODING_ASCII, &r_enc);
    else
    {
        encoding_t *encoding = ore_encoding(NULL, NULL, &r_enc);
        encoding->assumed = (r_enc == CE_NATIVE);
        return encoding;
    }
}

// Check whether a text encoding is consistent with a regex encoding
Rboolean ore_consistent_encodings (encoding_t *text_encoding, OnigEncoding regex_enc)
{
    const OnigEncoding text_enc = text_encoding->onig_enc;
    
    // ASCII is used as an "unknown" or default encoding, and native strings may be in any encoding, so they are considered consistent with any regex encoding that is ASCII-compatible (i.e. not UTF-16 or UTF-32)
    if (text_enc == regex_enc)
        return TRUE;
    else if (text_enc == ONIG_ENCODING_ASCII || text_encoding->assumed)
        return (ONIGENC_MBC_MINLEN(regex_enc) == 1);
    else if (regex_enc == ONIG_ENCODING_ASCII)
        return (ONIGENC_MBC_MINLEN(text_enc) == 1);
    else
        return FALSE;
}

// Obtain a handle for converting text to UTF-8, if that is needed (otherwise NULL)
// NB: The caller should read encoding->r_enc after calling this function, since it is changed if conversion turns out to be impossible
void * ore_iconv_handle (encoding_t *encoding)
{
    if (encoding == NULL || !encoding->convert)
        return NULL;
    
    void *iconv_handle = Riconv_open("UTF-8", encoding->name);
    if (iconv_handle == (void *) -1)
    {
        // Fall back to returning the text as-is, and don't try again for this encoding object
        warning("Text in encoding \"%s\" cannot be converted to UTF-8, and will be returned unmodified", encoding->name);
        encoding->convert = FALSE;
        encoding->r_enc = CE_NATIVE;
        return NULL;
    }
    
    return iconv_handle;
}

// Wrapper around Riconv, to convert between encodings
// The input has the specified length (which may include nul bytes, as in UTF-16), and the result is nul-terminated with its length stored in "new_len"
// Any bytes that are invalid in the source encoding are replaced with '?'
const char * ore_iconv (void *iconv_handle, const char *old, const size_t old_len, size_t *new_len)
{
    if (iconv_handle != NULL)
    {
        size_t old_size = old_len;
        // Each input byte produces at most one character, and a UTF-8 character is at most four bytes
        size_t new_size = old_size * 4;
        char *buffer = R_alloc(new_size+1, 1);
        char *buffer_start = buffer;
        while (old_size > 0)
        {
            if (Riconv(iconv_handle, &old, &old_size, &buffer, &new_size) != (size_t) -1 || new_size == 0)
                break;
            
            // Conversion stopped at an invalid or incomplete sequence, so substitute for one byte and carry on
            *(buffer++) = '?';
            new_size--;
            old++;
            old_size--;
        }
        *buffer = '\0';
        *new_len = (size_t) (buffer - buffer_start);
        return buffer_start;
    }
    else
    {
        *new_len = old_len;
        return old;
    }
}

// Close the specified handle
void ore_iconv_done (void *iconv_handle)
{
    if (iconv_handle != NULL)
        Riconv_close(iconv_handle);
}

// Helper functions to read a chunk of data from a file or connection
static size_t ore_read_file (void *handle, void *buffer, size_t bytes)
{
    FILE *file = (FILE *) handle;
    return fread(buffer, 1, bytes, file);
}

#ifdef USING_CONNECTIONS
static size_t ore_read_connection (void *handle, void *buffer, size_t bytes)
{
    Rconnection connection = (Rconnection) handle;
    if (!connection->isopen)
        connection->open(connection);
    return R_ReadConnection(connection, buffer, bytes);
}
#endif

// Create a text object from an R object: a file path, connection or literal character vector
text_t * ore_text (SEXP text_)
{
    text_t *text = (text_t *) R_alloc(1, sizeof(text_t));
    text->object = text_;
    text->length = 1;
    
    if (inherits(text_, "orefile"))
    {
        const SEXP encoding_name = getAttrib(text_, install("encoding"));
        text->encoding = ore_encoding(CHAR(STRING_ELT(encoding_name,0)), NULL, NULL);
        text->source = FILE_SOURCE;
        text->handle = fopen(CHAR(STRING_ELT(text_,0)), "rb");
        if (text->handle == NULL)
            error("Could not open file %s", CHAR(STRING_ELT(text_,0)));
    }
#ifdef USING_CONNECTIONS
    else if (inherits(text_, "connection"))
    {
        Rconnection connection = R_GetConnection(text_);
        text->encoding = ore_encoding(connection->encname, NULL, NULL);
        text->source = CONNECTION_SOURCE;
        text->handle = connection;
    }
#endif
    else if (isString(text_))
    {
        text->length = length(text_);
        text->source = VECTOR_SOURCE;
        text->handle = NULL;
        
        // The overall encoding (used for compiling regexes given as strings) is that of the first element that isn't pure ASCII, if any
        text->encoding = NULL;
        for (size_t i=0; i<text->length; i++)
        {
            SEXP element = STRING_ELT(text_, i);
            if (element != NA_STRING && !ore_is_ascii(CHAR(element)))
            {
                text->encoding = ore_string_encoding(element);
                break;
            }
        }
        if (text->encoding == NULL)
        {
            cetype_t encoding = CE_NATIVE;
            text->encoding = ore_encoding(NULL, ONIG_ENCODING_ASCII, &encoding);
        }
    }
    else
        error("The specified object cannot be used as a text source");
    
    return text;
}

// Extract the text element with the specified index
// For file and connection sources, index is ignored but reading may be incremental, passing in the previously read fragment
text_element_t * ore_text_element (text_t *text, const size_t index, const Rboolean incremental, text_element_t *previous)
{
    if (text == NULL)
        return NULL;
    
    text_element_t *element = (text_element_t *) R_alloc(1, sizeof(text_element_t));
    element->incomplete = FALSE;
    
    if (text->source == VECTOR_SOURCE)
    {
        SEXP str_element = STRING_ELT(text->object, index);
        if (str_element == NA_STRING)
            return NULL;
        const char *string = CHAR(str_element);
        element->start = string;
        element->end = string + strlen(string);
        element->encoding = ore_string_encoding(str_element);
    }
    else
    {
        char *buffer, *ptr;
        size_t buffer_size;
        if (incremental && previous != NULL)
        {
            buffer_size = (size_t) (previous->end - previous->start);
            buffer = ore_realloc(previous->start, 2 * buffer_size, buffer_size, 1);
            ptr = buffer + buffer_size;
        }
        else
        {
            buffer_size = FILE_BUFFER_SIZE;
            buffer = (char *) R_alloc(buffer_size, 1);
            ptr = buffer;
        }
        
        while (TRUE)
        {
            size_t bytes_read = 0;
            if (text->source == FILE_SOURCE)
                bytes_read = ore_read_file(text->handle, ptr, buffer_size);
#ifdef USING_CONNECTIONS
            else if (text->source == CONNECTION_SOURCE)
                bytes_read = ore_read_connection(text->handle, ptr, buffer_size);
#endif
            ptr += bytes_read;
            
            const Rboolean done = bytes_read < buffer_size;
            if (done)
            {
                // Append a nul, as a safeguard against string functions continuing beyond EOF, but don't include it in the text
                // There will always be space since the number of bytes read is strictly less than the buffer size
                *ptr = '\0';
                break;
            }
            else if (incremental)
            {
                element->incomplete = !done;
                break;
            }
            else
            {
                // NB: Any pointer arithmetic must happen before the buffer is reallocated
                buffer_size = (size_t) (ptr - buffer);
                buffer = ore_realloc(buffer, 2 * buffer_size, buffer_size, 1);
                ptr = buffer + buffer_size;
            }
        }
        
        element->start = buffer;
        element->end = ptr;
        element->encoding = text->encoding;
    }
    
    return element;
}

// Convert a text element to a CHARSXP (single string)
SEXP ore_text_element_to_rchar (text_element_t *element)
{
    return ore_bytes_to_rchar(element->start, (size_t) (element->end - element->start), element->encoding);
}

// Convert a C string to a CHARSXP, changing encoding if necessary
SEXP ore_string_to_rchar (const char *string, encoding_t *encoding)
{
    return ore_bytes_to_rchar(string, strlen(string), encoding);
}

// Convert a sequence of bytes of known length to a CHARSXP, changing encoding if necessary
SEXP ore_bytes_to_rchar (const char *bytes, const size_t length, encoding_t *encoding)
{
    void *iconv_handle = ore_iconv_handle(encoding);
    SEXP result = PROTECT(ore_convert_bytes(iconv_handle, bytes, length, encoding->r_enc));
    ore_iconv_done(iconv_handle);
    
    UNPROTECT(1);
    return result;
}

// Convert a sequence of bytes of known length to a CHARSXP, using an existing iconv handle (which may be NULL if no conversion is needed)
// NB: R strings cannot contain nul bytes, so the result is truncated at the first one, if any (after any conversion)
SEXP ore_convert_bytes (void *iconv_handle, const char *bytes, const size_t length, const cetype_t r_enc)
{
    size_t new_length;
    const char *converted = ore_iconv(iconv_handle, bytes, length, &new_length);
    
    const char *nul = memchr(converted, '\0', new_length);
    if (nul != NULL)
        new_length = (size_t) (nul - converted);
    
    return mkCharLenCE(converted, (int) new_length, r_enc);
}

// Tidy up a text object, where needed
void ore_text_done (text_t *text)
{
    // R handles closing connections, but plain files need to be closed manually
    if (text != NULL && text->source == FILE_SOURCE)
        fclose((FILE *) text->handle);
}
