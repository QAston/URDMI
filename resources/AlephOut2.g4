grammar AlephOut;

file:        line*;
line:        ( line_text NEWLINE ) | ( line_text EOF ) | NEWLINE;

//any_section_header: '[' HEADER_TEXT*? ']';

line_text: SECTION_HEADER | (SECTION_HEADER? TEXT);


NEWLINE :   ('\r'? '\n') ;
SECTION_HEADER: '[' PRINTABLE*? ']';
TEXT: PRINTABLE+;
PRINTABLE    : ( '\t' | '\u0020' .. '\u007f') ;


