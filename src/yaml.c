#include "yaml.h"

/* Accessors */

yaml_parser_t* make_parser(const char* str, size_t len) {
  yaml_parser_t* parser = (yaml_parser_t*)malloc(sizeof(yaml_parser_t));
  if(yaml_parser_initialize(parser)) {
    yaml_parser_set_input_string(parser,(const unsigned char*)str, len);
    return parser;
  }
  else
    return NULL;
}

void delete_parser(yaml_parser_t* parser) {
  yaml_parser_delete(parser);
}

yaml_event_t* make_event() {
  return (yaml_event_t*)malloc(sizeof(yaml_event_t));
}

void delete_event(yaml_event_t* event) {
  yaml_event_delete(event);
}

int parse_event(yaml_parser_t* parser, yaml_event_t* event) {
  return yaml_parser_parse(parser, event);
}

int event_type(yaml_event_t* event) {
  return event->type;
}

const char* event_value(yaml_event_t* event) {
  return (const char*)event->data.scalar.value;
}

const char* event_anchor(yaml_event_t* event) {
  return (const char*)event->data.scalar.anchor;
}

/* Enum values */

int enum_scalar() { return YAML_SCALAR_EVENT; }
int enum_alias() { return YAML_ALIAS_EVENT; }
int enum_seq_start() { return YAML_SEQUENCE_START_EVENT; }
int enum_seq_end() { return YAML_SEQUENCE_END_EVENT; } 
int enum_map_start() { return YAML_MAPPING_START_EVENT; }
int enum_map_end() { return YAML_MAPPING_END_EVENT; }
int enum_doc_start() { return YAML_DOCUMENT_START_EVENT; }
int enum_doc_end() { return YAML_DOCUMENT_END_EVENT; }
int enum_stream_start() { return YAML_STREAM_START_EVENT; }
int enum_stream_end() { return YAML_STREAM_END_EVENT; }
