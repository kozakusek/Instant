.class public test07
.super java/lang/Object

.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 2
.limit stack 3
  iconst_1
  istore_1
  ldc 65537
  iload_1
  iadd
  istore_1
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload_1
  invokevirtual java/io/PrintStream/println(I)V
  return
.end method
