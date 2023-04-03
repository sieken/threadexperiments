CC=gcc

%.o:%.c
	$(CC) -c -o $@ $<

token: token.o
	$(CC) -o token token.o

.PHONY: run_token_c run_token_erl clean

run_token_c: token
	./token 1000 1000

run_token_erl: token.erl
	erl -s token.erl 1000 1000

clean:
	rm -f token token.o