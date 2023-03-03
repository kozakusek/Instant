.class public test06
.super java/lang/Object

.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 15
.limit stack 4
  iconst_0
  istore_1
  iconst_1
  istore_2
  iconst_0
  istore_3
  iconst_1
  istore 4
  iconst_0
  istore 5
  iconst_1
  istore 6
  iconst_0
  istore 7
  iconst_1
  istore 8
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload_3
  iload 4
  imul
  iload 7
  iload 8
  iadd
  iload 6
  iadd
  iload 5
  iadd
  iadd
  iload_1
  iload_2
  imul
  iadd
  invokevirtual java/io/PrintStream/println(I)V
  iconst_1
  istore_1
  iconst_2
  istore_2
  iconst_1
  istore_3
  iconst_2
  istore 4
  iconst_1
  istore 5
  iconst_2
  istore 6
  iconst_1
  istore 7
  iconst_2
  istore 8
  iconst_1
  istore 9
  iconst_2
  istore 10
  iconst_1
  istore 11
  iconst_2
  istore 12
  iconst_1
  istore 13
  iconst_2
  istore 14
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload 10
  iconst_2
  idiv
  iload 13
  iload 14
  iadd
  iload 12
  iadd
  iload 11
  iadd
  iadd
  iload 9
  iadd
  iload 8
  iadd
  iload 7
  iadd
  iload 6
  iadd
  iload 5
  iadd
  iload 4
  iadd
  iload_3
  iadd
  iload_2
  iconst_2
  idiv
  iadd
  iconst_2
  iload_1
  imul
  iadd
  bipush 10
  idiv
  invokevirtual java/io/PrintStream/println(I)V
  return
.end method
