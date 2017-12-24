#include "../cbits/sqlite3.c"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define GUARD(stmt, msg) \
    do {                      \
        int err = stmt;       \
        if (err != SQLITE_OK) \
        {                     \
            printf(msg);      \
            exit(1);          \
        }                     \
    } while(0)

static void query(sqlite3* conn)
{
    int ret;
    const int N = 10000;
    long sum = 0;
    long total_sum = N * 27;
    sqlite3_stmt* stmt;

    GUARD(sqlite3_prepare_v2(conn, "SELECT id,name,age,created_at FROM person", -1, &stmt, NULL),
          "prepare_v2 failed");

    while((ret = sqlite3_step(stmt)) != SQLITE_DONE)
    {
        if (ret != SQLITE_ROW)
        {
            printf("failed step");
            exit(1);
        }

        sum += sqlite3_column_int(stmt, 2);
    }

    if (sum != total_sum)
    {
      printf("expecting sum: %ld got: %ld \n", total_sum, sum);
      exit(1);
    }

    sqlite3_finalize(stmt);
}

int main(void)
{
  int ITERS = 1;
  int i;
  sqlite3* conn;

  GUARD(sqlite3_open("../sample.db", &conn), "open failed");


  for (i = 0; i < ITERS; i++)
    query(conn);

  sqlite3_close(conn);
  return 0;
}
