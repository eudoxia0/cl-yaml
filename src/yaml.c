#include "yaml.h"

/* TokenList */

TokenList createTokenList(void) {
  TokenList list;
  list.data = (Token*)malloc(sizeof(Token)*LIST_CHUNK_SIZE);
  list.len = 0;
  list.cap = LIST_CHUNK_SIZE;
  return list;
}

void appendToken(TokenList* list, Token tok) {
  if((list->len + 1) == list->cap) {
    list->cap += LIST_CHUNK_SIZE;
    list->data = (Token*)realloc(list->data,list->cap);
  }
  list->data[list->len] = tok;
  list->len++;
}

void destroyTokenList(TokenList list) {
  free(list.data);
}

/* DocumentList */

DocumentList createDocumentList(void) {
  DocumentList docs;
  docs.data = (TokenList*)malloc(sizeof(TokenList)*LIST_CHUNK_SIZE);
  docs.len = 0;
  docs.cap = LIST_CHUNK_SIZE;
  return docs;
}

DocumentList tok_err(const char* msg) {
  /* Signals a tokenization error */
  DocumentList docs;
  docs.err = msg;
  return docs;
}

void appendDocument(DocumentList* docs, TokenList list) {
  if((docs->len + 1) == docs->cap) {
    docs->cap += LIST_CHUNK_SIZE;
    docs->data = (TokenList*)realloc(docs->data,docs->cap);
  }
  docs->data[docs->len] = list;
  docs->len++;
}

void destroyDocumentList(DocumentList docs) {
  size_t i = 0;
  for(; i < docs.len; i++)
    destroyTokenList(docs.data[i]);
}

/* Tokenization */

DocumentList tokenize(const char* str, size_t len) {
  /* Initialization */
  yaml_parser_t parser;
  yaml_event_t  event;
  DocumentList docs = createDocumentList();

  if(str == NULL)
    return tok_err("Can't parse a null string.");
  if(len == 0)
    return tok_err("Can't parse a string with length zero.");
  if(!yaml_parser_initialize(&parser))
    return tok_err("Could not initialize parser.");
  appendDocument(&docs, createTokenList());
  yaml_parser_set_input_string(&parser, (const unsigned char*)str, len);

  while(event.type != YAML_STREAM_END_EVENT) {
    Token tok;
    if(!yaml_parser_parse(&parser, &event)) {
      return tok_err("Parsing error"/*parser.error*/);
    }
    tok.type = event.type;
    switch(event.type) {
    case YAML_SCALAR_EVENT:
      tok.value = (const char*)event.data.scalar.value;
      tok.anchor = (const char*)event.data.scalar.anchor;
      break;
    case YAML_ALIAS_EVENT:
      tok.value = (const char*)event.data.alias.anchor;
      break;
    case YAML_DOCUMENT_START_EVENT:
      /* Add a new document to the list */
      appendDocument(&docs,createTokenList());
      break;
    default:
      /* The token only carries type information */
      break;
    }
    appendToken(&docs.data[docs.len-1],tok);
    if(event.type != YAML_STREAM_END_EVENT)
      yaml_event_delete(&event);
  }
  yaml_event_delete(&event);
  
  /* Finalize */
  yaml_parser_delete(&parser);

  return docs;
}
