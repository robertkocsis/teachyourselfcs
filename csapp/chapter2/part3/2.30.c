
int tadd_ok(int x, int y)
{
    if (x >= 0 && y >= 0 && x + y < 0)
    {
        return 0;
    }
    else if (x < 0 && y < 0 && x + y >= 0)
    {
        return 0;
    }
    else
    {
        return 1;
    }
}
