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

.method public static main([Ljava/lang/Object;Ljava/lang/Integer;)Ljava/lang/Integer;
    .registers 4
    .param p0, "arcd"    # [Ljava/lang/Object;
    .param p1, "fp"    # Ljava/lang/Integer;

    .prologue
    .line 9
    const-string v0, "Hello, Android World!\n"

    invoke-static {v0}, Luhideyuki/daat/DaatRuntime;->print(Ljava/lang/String;)V

    .line 10
    invoke-static {p1}, Luhideyuki/daat/DaatRuntime;->tostring(Ljava/lang/Integer;)Ljava/lang/String;

    move-result-object v0

    invoke-static {v0}, Luhideyuki/daat/DaatRuntime;->print(Ljava/lang/String;)V

    .line 11
    new-instance v0, Ljava/lang/Integer;

    const/4 v1, 0x0

    invoke-direct {v0, v1}, Ljava/lang/Integer;-><init>(I)V

    return-object v0
.end method
