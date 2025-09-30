
char *String$index(char **, int) {

}

int main() {

  struct {
    char *input;
    int pos;
  } *state = 0;

  char buffer[2] = (char[2]){(*(String$index((&(state->input)), state->pos))), 0x0, };
}