.class public Luhideyuki/daat/DaatRuntime;
.super Landroid/app/Activity;
.source "DaatRuntime.java"


# static fields
.field static tv:Landroid/widget/TextView;


# direct methods
.method public constructor <init>()V
    .registers 1

    .prologue
    .line 10
    invoke-direct {p0}, Landroid/app/Activity;-><init>()V

    return-void
.end method

.method public static print(Ljava/lang/String;)V
    .registers 2
    .param p0, "s"    # Ljava/lang/String;

    .prologue
    .line 25
    sget-object v0, Luhideyuki/daat/DaatRuntime;->tv:Landroid/widget/TextView;

    invoke-virtual {v0, p0}, Landroid/widget/TextView;->append(Ljava/lang/CharSequence;)V

    .line 26
    return-void
.end method


# virtual methods
.method public onCreate(Landroid/os/Bundle;)V
    .registers 3
    .param p1, "savedInstanceState"    # Landroid/os/Bundle;

    .prologue
    .line 17
    invoke-super {p0, p1}, Landroid/app/Activity;->onCreate(Landroid/os/Bundle;)V

    .line 18
    const/high16 v0, 0x7f030000

    invoke-virtual {p0, v0}, Luhideyuki/daat/DaatRuntime;->setContentView(I)V

    .line 19
    const/high16 v0, 0x7f050000

    invoke-virtual {p0, v0}, Luhideyuki/daat/DaatRuntime;->findViewById(I)Landroid/view/View;

    move-result-object v0

    check-cast v0, Landroid/widget/TextView;

    sput-object v0, Luhideyuki/daat/DaatRuntime;->tv:Landroid/widget/TextView;

    .line 20
    invoke-static {}, Luhideyuki/daat/DaatProg;->main()V

    .line 21
    return-void
.end method
