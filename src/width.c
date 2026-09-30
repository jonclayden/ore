#include <string.h>

#include "onigmo.h"
#include "width.h"

// Ranges of code points with East Asian Width "W" (wide) or "F" (fullwidth), which take up two terminal columns
// This includes emoji with default emoji presentation, and unassigned code points in the CJK ideograph blocks and planes 2 and 3
// NB: This table is generated from the Unicode Character Database by tools/width-table.R, so should not be edited by hand
// BEGIN GENERATED TABLE
// Unicode 17.0.0: 123 ranges
static const OnigCodePoint wide_ranges[][2] = {
    { 0x1100, 0x115F },
    { 0x231A, 0x231B },
    { 0x2329, 0x232A },
    { 0x23E9, 0x23EC },
    { 0x23F0, 0x23F0 },
    { 0x23F3, 0x23F3 },
    { 0x25FD, 0x25FE },
    { 0x2614, 0x2615 },
    { 0x2630, 0x2637 },
    { 0x2648, 0x2653 },
    { 0x267F, 0x267F },
    { 0x268A, 0x268F },
    { 0x2693, 0x2693 },
    { 0x26A1, 0x26A1 },
    { 0x26AA, 0x26AB },
    { 0x26BD, 0x26BE },
    { 0x26C4, 0x26C5 },
    { 0x26CE, 0x26CE },
    { 0x26D4, 0x26D4 },
    { 0x26EA, 0x26EA },
    { 0x26F2, 0x26F3 },
    { 0x26F5, 0x26F5 },
    { 0x26FA, 0x26FA },
    { 0x26FD, 0x26FD },
    { 0x2705, 0x2705 },
    { 0x270A, 0x270B },
    { 0x2728, 0x2728 },
    { 0x274C, 0x274C },
    { 0x274E, 0x274E },
    { 0x2753, 0x2755 },
    { 0x2757, 0x2757 },
    { 0x2795, 0x2797 },
    { 0x27B0, 0x27B0 },
    { 0x27BF, 0x27BF },
    { 0x2B1B, 0x2B1C },
    { 0x2B50, 0x2B50 },
    { 0x2B55, 0x2B55 },
    { 0x2E80, 0x2E99 },
    { 0x2E9B, 0x2EF3 },
    { 0x2F00, 0x2FD5 },
    { 0x2FF0, 0x303E },
    { 0x3041, 0x3096 },
    { 0x3099, 0x30FF },
    { 0x3105, 0x312F },
    { 0x3131, 0x318E },
    { 0x3190, 0x31E5 },
    { 0x31EF, 0x321E },
    { 0x3220, 0x3247 },
    { 0x3250, 0xA48C },
    { 0xA490, 0xA4C6 },
    { 0xA960, 0xA97C },
    { 0xAC00, 0xD7A3 },
    { 0xF900, 0xFAFF },
    { 0xFE10, 0xFE19 },
    { 0xFE30, 0xFE52 },
    { 0xFE54, 0xFE66 },
    { 0xFE68, 0xFE6B },
    { 0xFF01, 0xFF60 },
    { 0xFFE0, 0xFFE6 },
    { 0x16FE0, 0x16FE4 },
    { 0x16FF0, 0x16FF6 },
    { 0x17000, 0x18CD5 },
    { 0x18CFF, 0x18D1E },
    { 0x18D80, 0x18DF2 },
    { 0x1AFF0, 0x1AFF3 },
    { 0x1AFF5, 0x1AFFB },
    { 0x1AFFD, 0x1AFFE },
    { 0x1B000, 0x1B122 },
    { 0x1B132, 0x1B132 },
    { 0x1B150, 0x1B152 },
    { 0x1B155, 0x1B155 },
    { 0x1B164, 0x1B167 },
    { 0x1B170, 0x1B2FB },
    { 0x1D300, 0x1D356 },
    { 0x1D360, 0x1D376 },
    { 0x1F004, 0x1F004 },
    { 0x1F0CF, 0x1F0CF },
    { 0x1F18E, 0x1F18E },
    { 0x1F191, 0x1F19A },
    { 0x1F200, 0x1F202 },
    { 0x1F210, 0x1F23B },
    { 0x1F240, 0x1F248 },
    { 0x1F250, 0x1F251 },
    { 0x1F260, 0x1F265 },
    { 0x1F300, 0x1F320 },
    { 0x1F32D, 0x1F335 },
    { 0x1F337, 0x1F37C },
    { 0x1F37E, 0x1F393 },
    { 0x1F3A0, 0x1F3CA },
    { 0x1F3CF, 0x1F3D3 },
    { 0x1F3E0, 0x1F3F0 },
    { 0x1F3F4, 0x1F3F4 },
    { 0x1F3F8, 0x1F43E },
    { 0x1F440, 0x1F440 },
    { 0x1F442, 0x1F4FC },
    { 0x1F4FF, 0x1F53D },
    { 0x1F54B, 0x1F54E },
    { 0x1F550, 0x1F567 },
    { 0x1F57A, 0x1F57A },
    { 0x1F595, 0x1F596 },
    { 0x1F5A4, 0x1F5A4 },
    { 0x1F5FB, 0x1F64F },
    { 0x1F680, 0x1F6C5 },
    { 0x1F6CC, 0x1F6CC },
    { 0x1F6D0, 0x1F6D2 },
    { 0x1F6D5, 0x1F6D8 },
    { 0x1F6DC, 0x1F6DF },
    { 0x1F6EB, 0x1F6EC },
    { 0x1F6F4, 0x1F6FC },
    { 0x1F7E0, 0x1F7EB },
    { 0x1F7F0, 0x1F7F0 },
    { 0x1F90C, 0x1F93A },
    { 0x1F93C, 0x1F945 },
    { 0x1F947, 0x1F9FF },
    { 0x1FA70, 0x1FA7C },
    { 0x1FA80, 0x1FA8A },
    { 0x1FA8E, 0x1FAC6 },
    { 0x1FAC8, 0x1FAC8 },
    { 0x1FACD, 0x1FADC },
    { 0x1FADF, 0x1FAEA },
    { 0x1FAEF, 0x1FAF8 },
    { 0x20000, 0x2FFFD },
    { 0x30000, 0x3FFFD }
};
// END GENERATED TABLE

// Properties of characters that take up no space of their own: nonspacing and enclosing marks, format characters (such as zero-width spaces and joiners), and Hangul medial vowels and final consonants (which combine with an initial consonant)
// These are looked up in Oniguruma's Unicode tables, so they are kept in step with those
static const char *zero_width_properties[] = { "Mn", "Me", "Cf", "Grapheme_Cluster_Break=V", "Grapheme_Cluster_Break=T" };
#define N_ZERO_WIDTH_PROPERTIES (sizeof(zero_width_properties) / sizeof(zero_width_properties[0]))

// Oniguruma character types corresponding to the properties, looked up when first needed
static int zero_width_ctypes[N_ZERO_WIDTH_PROPERTIES];
static int zero_width_ctypes_found = 0;

// Work out the display width of a Unicode code point, in terminal columns
// The result is -1 for control characters, 0 for characters that combine with the previous one or are otherwise invisible, 2 for wide characters, and 1 otherwise
int ore_code_width (const OnigCodePoint code)
{
    // Nul, and C0 and C1 control characters
    if (code == 0)
        return 0;
    else if (code < 0x20 || (code >= 0x7f && code < 0xa0))
        return -1;
    else if (code < 0x7f)
        return 1;
    
    // The soft hyphen is a format character, but is usually displayed
    if (code == 0xad)
        return 1;
    
    if (!zero_width_ctypes_found)
    {
        for (size_t i=0; i<N_ZERO_WIDTH_PROPERTIES; i++)
        {
            const UChar *name = (const UChar *) zero_width_properties[i];
            zero_width_ctypes[i] = ONIGENC_PROPERTY_NAME_TO_CTYPE(ONIG_ENCODING_UTF8, (UChar *) name, (UChar *) name + strlen((const char *) name));
        }
        zero_width_ctypes_found = 1;
    }
    
    // A negative character type indicates that the property wasn't found, which shouldn't happen
    for (size_t i=0; i<N_ZERO_WIDTH_PROPERTIES; i++)
    {
        if (zero_width_ctypes[i] >= 0 && ONIGENC_IS_CODE_CTYPE(ONIG_ENCODING_UTF8, code, (unsigned int) zero_width_ctypes[i]))
            return 0;
    }
    
    // Binary search in the table of wide ranges
    size_t lower = 0, upper = sizeof(wide_ranges) / sizeof(wide_ranges[0]);
    while (lower < upper)
    {
        const size_t middle = (lower + upper) / 2;
        if (code < wide_ranges[middle][0])
            upper = middle;
        else if (code > wide_ranges[middle][1])
            lower = middle + 1;
        else
            return 2;
    }
    
    return 1;
}
