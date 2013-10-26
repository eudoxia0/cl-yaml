#include <yaml.h>

/* Accessors */

yaml_parser_t* make_parser(const char* str, size_t len);
void delete_parser(yaml_parser_t* parser);

yaml_event_t* make_event();
void delete_event(yaml_event_t* event);

int parse_event(yaml_parser_t* parser, yaml_event_t* event);

int event_type(yaml_event_t* event);
const char* event_value(yaml_event_t* event);
const char* event_anchor(yaml_event_t* event);

/* Enum values */

int enum_scalar();
int enum_alias();
int enum_seq_start();
int enum_seq_end();
int enum_map_start();
int enum_map_end();
int enum_doc_start();
int enum_doc_end();
int enum_stream_start();
int enum_stream_end();
