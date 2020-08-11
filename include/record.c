#define record(name, size) \
  int current_record_pos = 0; \
  const int this_type = next_type++; \
  immutable void(^ cognate_function_ ## name)(void) = \
  ^{ \
    cognate_object *rec = (cognate_object*) malloc (sizeof(cognate_object) * size); \
    for (int i = 0; i < size; ++i) \
    { \
      rec[i] = pop_any(); \
    } \
    push_any((cognate_object){.type=this_type, .record=rec}); \
  }; \
  immutable void(^ cognate_function_is_ ## name)(void) = \
  ^{ \
    push(boolean, pop_any().type == this_type); \
  }

// Field definitions MUST come directly after record definitions
// because each record definition increments the type counter.

#define field(name) \
  const int record_ ## name ## _pos = ++current_record_pos; \
  immutable void(^ cognate_function_ ## name)(void) = \
  ^{ \
    cognate_object rec = pop_any(); \
    if (rec.type == this_type) { push_any(rec.record[record_ ## name ## _pos]); } \
    else { throw_error("No field '"#name"' in record!"); } \
  };


