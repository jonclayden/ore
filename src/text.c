#include <string.h>

#include <R.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <R_ext/Riconv.h>
#include <R_ext/Connections.h>

#include "text.h"

// If R is recent enough for R_GetConnection() to be available, and the connections API is the expected version, support reading from connections
#if defined(R_VERSION) && R_VERSION >= R_Version(3,3,0) && defined(R_CONNECTIONS_VERSION) && R_CONNECTIONS_VERSION == 1
#define USING_CONNECTIONS
#endif

// Initial buffer size when reading from a file; scales exponentially
#define FILE_BUFFER_SIZE    1024

// Not strictly part of the API, but useful for case-insensitive string comparison
extern int onigenc_with_ascii_strnicmp (OnigEncoding enc, const UChar *p, const UChar *end, const UChar *sascii, int n);

int ore_strnicmp (const char *str1, const char *str2, size_t num)
{
    return onigenc_with_ascii_strnicmp(ONIG_ENCODING_ASCII, (const UChar *) str1, (const UChar *) str1 + num, (const UChar *) str2, num);
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

// Convert an encoding string to its Oniguruma equivalent
static OnigEncoding ore_name_to_onig_enc (const char *enc)
{
    if (ore_strnicmp(enc,"ASCII",5) == 0 || ore_strnicmp(enc,"US-ASCII",8) == 0)
        return ONIG_ENCODING_ASCII;
    else if (ore_strnicmp(enc,"UTF-8",5) == 0 || ore_strnicmp(enc,"UTF8",4) == 0)
        return ONIG_ENCODING_UTF8;
    else if (ore_strnicmp(enc,"ISO_8859-1",10) == 0 || ore_strnicmp(enc,"ISO-8859-1",10) == 0 || ore_strnicmp(enc,"ISO8859-1",9) == 0 || ore_strnicmp(enc,"LATIN1",6) == 0)
        return ONIG_ENCODING_ISO_8859_1;
    else if (ore_strnicmp(enc,"ISO_8859-2",10) == 0 || ore_strnicmp(enc,"ISO-8859-2",10) == 0 || ore_strnicmp(enc,"ISO8859-2",9) == 0 || ore_strnicmp(enc,"LATIN2",6) == 0)
        return ONIG_ENCODING_ISO_8859_2;
    else if (ore_strnicmp(enc,"ISO_8859-3",10) == 0 || ore_strnicmp(enc,"ISO-8859-3",10) == 0 || ore_strnicmp(enc,"ISO8859-3",9) == 0 || ore_strnicmp(enc,"LATIN3",6) == 0)
        return ONIG_ENCODING_ISO_8859_3;
    else if (ore_strnicmp(enc,"ISO_8859-4",10) == 0 || ore_strnicmp(enc,"ISO-8859-4",10) == 0 || ore_strnicmp(enc,"ISO8859-4",9) == 0 || ore_strnicmp(enc,"LATIN4",6) == 0)
        return ONIG_ENCODING_ISO_8859_4;
    else if (ore_strnicmp(enc,"ISO_8859-5",10) == 0 || ore_strnicmp(enc,"ISO-8859-5",10) == 0 || ore_strnicmp(enc,"ISO8859-5",9) == 0 || ore_strnicmp(enc,"LATIN5",6) == 0)
        return ONIG_ENCODING_ISO_8859_5;
    else if (ore_strnicmp(enc,"ISO_8859-6",10) == 0 || ore_strnicmp(enc,"ISO-8859-6",10) == 0 || ore_strnicmp(enc,"ISO8859-6",9) == 0 || ore_strnicmp(enc,"LATIN6",6) == 0)
        return ONIG_ENCODING_ISO_8859_6;
    else if (ore_strnicmp(enc,"ISO_8859-7",10) == 0 || ore_strnicmp(enc,"ISO-8859-7",10) == 0 || ore_strnicmp(enc,"ISO8859-7",9) == 0 || ore_strnicmp(enc,"LATIN7",6) == 0)
        return ONIG_ENCODING_ISO_8859_7;
    else if (ore_strnicmp(enc,"ISO_8859-8",10) == 0 || ore_strnicmp(enc,"ISO-8859-8",10) == 0 || ore_strnicmp(enc,"ISO8859-8",9) == 0 || ore_strnicmp(enc,"LATIN8",6) == 0)
        return ONIG_ENCODING_ISO_8859_8;
    else if (ore_strnicmp(enc,"ISO_8859-9",10) == 0 || ore_strnicmp(enc,"ISO-8859-9",10) == 0 || ore_strnicmp(enc,"ISO8859-9",9) == 0 || ore_strnicmp(enc,"LATIN9",6) == 0)
        return ONIG_ENCODING_ISO_8859_9;
    else if (ore_strnicmp(enc,"ISO_8859-10",11) == 0 || ore_strnicmp(enc,"ISO-8859-10",11) == 0 || ore_strnicmp(enc,"ISO8859-10",10) == 0 || ore_strnicmp(enc,"LATIN10",7) == 0)
        return ONIG_ENCODING_ISO_8859_10;
    else if (ore_strnicmp(enc,"ISO_8859-11",11) == 0 || ore_strnicmp(enc,"ISO-8859-11",11) == 0 || ore_strnicmp(enc,"ISO8859-11",10) == 0 || ore_strnicmp(enc,"LATIN11",7) == 0)
        return ONIG_ENCODING_ISO_8859_11;
    else if (ore_strnicmp(enc,"ISO_8859-13",11) == 0 || ore_strnicmp(enc,"ISO-8859-13",11) == 0 || ore_strnicmp(enc,"ISO8859-13",10) == 0 || ore_strnicmp(enc,"LATIN13",7) == 0)
        return ONIG_ENCODING_ISO_8859_13;
    else if (ore_strnicmp(enc,"ISO_8859-14",11) == 0 || ore_strnicmp(enc,"ISO-8859-14",11) == 0 || ore_strnicmp(enc,"ISO8859-14",10) == 0 || ore_strnicmp(enc,"LATIN14",7) == 0)
        return ONIG_ENCODING_ISO_8859_14;
    else if (ore_strnicmp(enc,"ISO_8859-15",11) == 0 || ore_strnicmp(enc,"ISO-8859-15",11) == 0 || ore_strnicmp(enc,"ISO8859-15",10) == 0 || ore_strnicmp(enc,"LATIN15",7) == 0)
        return ONIG_ENCODING_ISO_8859_15;
    else if (ore_strnicmp(enc,"ISO_8859-16",11) == 0 || ore_strnicmp(enc,"ISO-8859-16",11) == 0 || ore_strnicmp(enc,"ISO8859-16",10) == 0 || ore_strnicmp(enc,"LATIN16",7) == 0)
        return ONIG_ENCODING_ISO_8859_16;
    else if (ore_strnicmp(enc,"UTF-16BE",8) == 0)
        return ONIG_ENCODING_UTF16_BE;
    else if (ore_strnicmp(enc,"UTF-16LE",8) == 0)
        return ONIG_ENCODING_UTF16_LE;
    else if (ore_strnicmp(enc,"UTF-32BE",8) == 0)
        return ONIG_ENCODING_UTF32_BE;
    else if (ore_strnicmp(enc,"UTF-32LE",8) == 0)
        return ONIG_ENCODING_UTF32_LE;
    else if (ore_strnicmp(enc,"BIG5",4) == 0 || ore_strnicmp(enc,"BIG-5",5) == 0 || ore_strnicmp(enc,"BIGFIVE",7) == 0 || ore_strnicmp(enc,"BIG-FIVE",8) == 0)
        return ONIG_ENCODING_BIG5;
    else if (ore_strnicmp(enc,"CP932",5) == 0)
        return ONIG_ENCODING_CP932;
    else if (ore_strnicmp(enc,"CP1250",6) == 0 || ore_strnicmp(enc,"WINDOWS-1250",12) == 0)
        return ONIG_ENCODING_WINDOWS_1250;
    else if (ore_strnicmp(enc,"CP1251",6) == 0 || ore_strnicmp(enc,"WINDOWS-1251",12) == 0)
        return ONIG_ENCODING_WINDOWS_1251;
    else if (ore_strnicmp(enc,"CP1252",6) == 0 || ore_strnicmp(enc,"WINDOWS-1252",12) == 0)
        return ONIG_ENCODING_WINDOWS_1252;
    else if (ore_strnicmp(enc,"CP1253",6) == 0 || ore_strnicmp(enc,"WINDOWS-1253",12) == 0)
        return ONIG_ENCODING_WINDOWS_1253;
    else if (ore_strnicmp(enc,"CP1254",6) == 0 || ore_strnicmp(enc,"WINDOWS-1254",12) == 0)
        return ONIG_ENCODING_WINDOWS_1254;
    else if (ore_strnicmp(enc,"CP1257",6) == 0 || ore_strnicmp(enc,"WINDOWS-1257",12) == 0)
        return ONIG_ENCODING_WINDOWS_1257;
    else if (ore_strnicmp(enc,"EUC-JP",6) == 0 || ore_strnicmp(enc,"EUCJP",5) == 0)
        return ONIG_ENCODING_EUC_JP;
    else if (ore_strnicmp(enc,"EUC-KR",6) == 0 || ore_strnicmp(enc,"EUCKR",5) == 0)
        return ONIG_ENCODING_EUC_KR;
    else if (ore_strnicmp(enc,"EUC-TW",6) == 0 || ore_strnicmp(enc,"EUCTW",5) == 0)
        return ONIG_ENCODING_EUC_TW;
    else if (ore_strnicmp(enc,"GB18030",7) == 0)
        return ONIG_ENCODING_GB18030;
    else if (ore_strnicmp(enc,"KOI8-R",6) == 0)
        return ONIG_ENCODING_KOI8_R;
    else if (ore_strnicmp(enc,"KOI8-U",4) == 0)
        return ONIG_ENCODING_KOI8_U;
    else if (ore_strnicmp(enc,"SHIFT_JIS",9) == 0 || ore_strnicmp(enc,"SHIFT-JIS",9) == 0 || ore_strnicmp(enc,"SJIS",4) == 0)
        return ONIG_ENCODING_SJIS;
    else
    {
        warning("Encoding \"%s\" is not supported by Oniguruma - using ASCII", enc);
        return ONIG_ENCODING_ASCII;
    }
}

// Create a consistent encoding structure from an existing type, propagating as closely as possible
encoding_t * ore_encoding (const char *name, OnigEncoding onig_enc, cetype_t *r_enc)
{
    // The fallback R encoding, where nothing else is marked
    cetype_t final_r_enc = CE_NATIVE;
    
    // If there's no Oniguruma encoding, work from a name, if available
    if (name != NULL && strlen(name) > 0 && onig_enc == NULL)
        onig_enc = ore_name_to_onig_enc(name);
    
    // If there's no R encoding, take it from the Oniguruma one
    if (r_enc == NULL)
    {
        if (onig_enc == ONIG_ENCODING_UTF8)
            final_r_enc = CE_UTF8;
        else if (onig_enc == ONIG_ENCODING_ISO_8859_1)
            final_r_enc = CE_LATIN1;
        else
            final_r_enc = CE_NATIVE;
    }
    
    // Propagate back from the R encoding if necessary, but R asserts very few encodings
    if (onig_enc == NULL && r_enc != NULL)
    {
        switch (*r_enc)
        {
            case CE_UTF8:   onig_enc = ONIG_ENCODING_UTF8;          break;
            case CE_LATIN1: onig_enc = ONIG_ENCODING_ISO_8859_1;    break;
            default:        onig_enc = ONIG_ENCODING_ASCII;         break;
        }
    }
    
    // Create, populate and return the encoding structure
    encoding_t *encoding = (encoding_t *) R_alloc(1, sizeof(encoding_t));
    if (name != NULL)
        strncpy(encoding->name, name, ORE_ENCODING_NAME_MAX_LEN);
    else
        encoding->name[0] = '\0';
    encoding->onig_enc = onig_enc;
    encoding->r_enc = final_r_enc;
    
    return encoding;
}

// Check whether the two specified encodings are consistent with one another
Rboolean ore_consistent_encodings (OnigEncoding first, OnigEncoding second)
{
    // ASCII is used as an "unknown" or default encoding, so it is considered consistent with everything
    return (first == second || first == ONIG_ENCODING_ASCII || second == ONIG_ENCODING_ASCII);
}

// Wrapper around Riconv, to convert between encodings
const char * ore_iconv (void *iconv_handle, const char *old)
{
    if (iconv_handle)
    {
        size_t old_size = strlen(old);
        size_t new_size = old_size * 6;
        char *buffer = R_alloc(new_size+1, 1);
        char *buffer_start = buffer;
        Riconv(iconv_handle, &old, &old_size, &buffer, &new_size);
        *buffer = '\0';
        return buffer_start;
    }
    else
        return old;
}

static size_t ore_read_file (void *handle, void *buffer, size_t bytes, int origin)
{
    FILE *file = (FILE *) handle;
    if (origin != SEEK_CUR)
        fseek(file, 0, origin);
    return fread(buffer, 1, bytes, file);
}

#ifdef USING_CONNECTIONS
static size_t ore_read_connection (void *handle, void *buffer, size_t bytes, int origin)
{
    Rconnection connection = (Rconnection) handle;
    if (origin != SEEK_CUR)
        error("Seeking is not supported for connections");
    return R_ReadConnection(connection, buffer, bytes);
}
#endif

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
        const Rconnection connection = R_GetConnection(text_);
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
        
        cetype_t encoding = CE_NATIVE;
        for (size_t i=0; i<text->length; i++)
        {
            const cetype_t current_encoding = getCharCE(STRING_ELT(text_, i));
            if (current_encoding == CE_UTF8 || current_encoding == CE_LATIN1)
            {
                encoding = current_encoding;
                break;
            }
        }
        text->encoding = ore_encoding(NULL, NULL, &encoding);
    }
    else
        error("The specified object cannot be used as a text source");
    
    return text;
}

text_element_t * ore_text_element (text_t *text, const size_t index)
{
    if (text == NULL)
        return NULL;
    
    text_element_t *element = (text_element_t *) R_alloc(1, sizeof(text_element_t));
    element->incomplete = FALSE;
    
    if (text->source == VECTOR_SOURCE)
    {
        const char *string = CHAR(STRING_ELT(text->object, index));
        cetype_t encoding = getCharCE(STRING_ELT(text->object, index));
        element->start = string;
        element->end = string + strlen(string);
        element->encoding = ore_encoding(NULL, NULL, &encoding);
    }
    // Incremental file source: we just want an initial subset of the file
    else if (text->source == FILE_SOURCE && index > 0)
    {
        size_t buffer_size = FILE_BUFFER_SIZE;
        for (size_t i=1; i<index; i++)
            buffer_size *= 2;
        
        char *buffer = (char *) R_alloc(buffer_size, 1);
        size_t bytes_read = ore_read_file(text->handle, buffer, buffer_size, SEEK_SET);
        
        element->start = buffer;
        element->end = buffer + bytes_read;
        element->encoding = text->encoding;
        element->incomplete = (bytes_read == buffer_size);
    }
    else
    {
        size_t old_buffer_size = 0;
        size_t buffer_size = FILE_BUFFER_SIZE;
        
        char *buffer = (char *) R_alloc(buffer_size, 1);
        char *ptr = buffer;
        while (TRUE)
        {
            const size_t n = buffer_size - old_buffer_size;
            size_t bytes_read;
            
            switch (text->source)
            {
                case FILE_SOURCE:
                bytes_read = ore_read_file(text->handle, ptr, n, SEEK_CUR);
                break;
                
#ifdef USING_CONNECTIONS
                case CONNECTION_SOURCE:
                bytes_read = ore_read_connection(text->handle, ptr, n, SEEK_CUR);
                break;
#endif
                
                case VECTOR_SOURCE:
                // Already handled – only here to silence compiler warnings
                break;
            }
            
            if (bytes_read < n)
            {
                ptr += bytes_read;
                break;
            }
            else
            {
                old_buffer_size = buffer_size;
                buffer_size *= 2;
                buffer = (char *) ore_realloc(buffer, buffer_size, old_buffer_size, 1);
                ptr = buffer + old_buffer_size;
            }
        }
        
        element->start = buffer;
        element->end = ptr;
        element->encoding = text->encoding;
    }
    
    return element;
}

SEXP ore_text_element_to_rchar (text_element_t *element)
{
    return ore_string_to_rchar(element->start, element->encoding);
}

SEXP ore_string_to_rchar (const char *string, encoding_t *encoding)
{
    void *iconv_handle = NULL;
    
    if (encoding != NULL)
    {
        if (ore_strnicmp(encoding->name, "native.enc", 10) == 0)
            iconv_handle = Riconv_open("UTF-8", "");
        else
            iconv_handle = Riconv_open("UTF-8", encoding->name);
        encoding->r_enc = CE_UTF8;
    }
    
    SEXP result = PROTECT(mkCharCE(ore_iconv(iconv_handle, string), encoding->r_enc));
    
    if (iconv_handle)
        Riconv_close(iconv_handle);
    
    UNPROTECT(1);
    return result;
}

void ore_text_done (text_t *text)
{
    // R handles closing connections, but plain files need to be closed manually
    if (text != NULL && text->source == FILE_SOURCE)
        fclose((FILE *) text->handle);
}
