#include <yaml.h>
#define LIST_CHUNK_SIZE 64

typedef struct {
  char* anchor;
  char* value;
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
  const char* err;
} TokenList;

TokenList* createTokenList(void);
void appendToken(TokenList* list, Token tok);
void destroyTokenList(TokenList* list);
const char* copy(const char* source);

TokenList* tokenize(const char* str, size_t len);


/* Accessors */

size_t list_len(TokenList* list);
Token* nth_tok(TokenList* list, size_t n);
const char* list_err(TokenList* list);
int tok_type(Token* tok);
const char* tok_value(Token* tok);
const char* tok_anchor(Token* tok);

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
