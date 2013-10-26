#include <stdlib.h>
#include <yaml.h>
#define LIST_CHUNK_SIZE 64

typedef struct {
  const char* anchor;
  const char* value;
  int type;
} Token;

/* To prevent frequent reallocations, lists initially
  allocates space for 64 objects, and adds another 64
  every time the list needs more space */

/* TokenList */

typedef struct {
  Token* data;
  size_t len;
  size_t cap;
} TokenList;

TokenList createTokenList(void);
void appendToken(TokenList list, Token tok);
void destroyTokenList(TokenList list);

/* DocumentList */

typedef struct {
  TokenList* data;
  size_t len;
  size_t cap;
  const char* err;
} DocumentList;

DocumentList createDocumentList(void);
DocumentList err(const char* msg);
void appendDocument(DocumentList docs, TokenList list);
void destroyDocumentList(DocumentList docs);

/* Tokenization */

DocumentList tokenize(const char* str, size_t len);
