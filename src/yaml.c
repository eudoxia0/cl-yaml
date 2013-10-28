#include "yaml.h"

TokenList* createTokenList(void) {
  TokenList* list = (TokenList*)malloc(sizeof(TokenList));
  list->data = (Token*)malloc(sizeof(Token)*LIST_CHUNK_SIZE);
  list->len = 0;
  list->cap = LIST_CHUNK_SIZE;
  list->err = NULL;
  return list;
}

void appendToken(TokenList* list, Token tok) {
  if((list->len + 1) == list->cap) {
    list->cap += LIST_CHUNK_SIZE;
    list->data = (Token*)realloc(list->data,list->cap*sizeof(Token));
  }
  list->data[list->len] = tok;
  list->len++;
}

void destroyTokenList(TokenList* list) {
  free(list->data);
  free(list);
}

const char* copy(const char* source) {
  return source;
}

TokenList* tokenize(const char* str, size_t len) {
  /* Initialization */
  yaml_parser_t parser;
  yaml_event_t  event;
  TokenList* tokens = createTokenList();

  if(str == NULL) {
    tokens->err = "Can't parse a null string.";
    return tokens;
  }
  if(len == 0) {
    tokens->err = "Can't parse a bstring with length zero.";
    return tokens;
  }
  if(!yaml_parser_initialize(&parser)) {
    tokens->err = "Could not initialize parser.";
    return tokens;
  }
  yaml_parser_set_input_string(&parser, (const unsigned char*)str, len);

  do {
    Token tok;
    size_t value_len = 0;
    size_t anchor_len = 0;
    tok.type = 0;
    tok.value = NULL;
    tok.anchor = NULL;
    if(!yaml_parser_parse(&parser, &event)) {
      tokens->err = "Parsing error";
      return tokens;
    }
    tok.type = event.type;
    switch(event.type) {
    case YAML_SCALAR_EVENT:
      tok.value = (char*)event.data.scalar.value;
      tok.anchor = (char*)event.data.scalar.anchor;
      if(tok.value) {
        value_len = strlen(tok.value);
        tok.value = malloc(value_len+1);
        strcpy(tok.value, (const char*)event.data.scalar.value);
      }
      if(tok.anchor) {
        anchor_len = strlen(tok.anchor);
        tok.anchor = malloc(anchor_len+1);
        strcpy(tok.anchor, (const char*)event.data.scalar.anchor);
      }
      break;
    case YAML_ALIAS_EVENT:
      tok.value = (char*)event.data.alias.anchor;
      break;
    default:
      /* The token only carries type information */
      break;
    }
    appendToken(tokens,tok);
    if(event.type != YAML_STREAM_END_EVENT)
      yaml_event_delete(&event);
  } while(event.type != YAML_STREAM_END_EVENT);
  
  /* Finalize */
  yaml_event_delete(&event);
  yaml_parser_delete(&parser);

  return tokens;
}


/* Accessors */

size_t list_len(TokenList* list) {
  return list->len;
}

Token* nth_tok(TokenList* list, size_t n) {
  return &list->data[n];
}

void destroy_nth_tok(TokenList* list, size_t n) {
  free(list->data[n].value);
  free(list->data[n].anchor);
}

const char* list_err(TokenList* list) {
  return list->err;
}

int tok_type(Token* tok) {
  return tok->type;
}

const char* tok_value(Token* tok) {
  return tok->value;
}

const char* tok_anchor(Token* tok) {
  return tok->anchor;
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
