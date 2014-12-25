.class public Luhideyuki/daat/DaatProg;
.super Ljava/lang/Object;
.source "DaatProg.java"


# direct methods
.method public constructor <init>()V
    .registers 1

    .prologue
    .line 5
    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    return-void
.end method

.method public static main()V
    .registers 1

    .prologue
    .line 9
    const-string v0, "Hello, world!\n"

    invoke-static {v0}, Luhideyuki/daat/DaatRuntime;->print(Ljava/lang/String;)V

    .line 10
    return-void
.end method
