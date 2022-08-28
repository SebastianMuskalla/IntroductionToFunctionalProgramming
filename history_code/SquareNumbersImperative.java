import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

public class SquareNumbersImperative
{
    public static void main (String[] args)
    {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        List<Integer> squares = firstSquares(n);
        System.out.println(squares.toString());
    }

    static int square(int n)
    {
        return n * n;
    }

    static List<Integer> firstSquares (int n)
    {
        List<Integer> numbers = new ArrayList<>();
        for (int i = 1; i <= n; i++)
        {
            numbers.add(i);
        }

        List<Integer> squares = new ArrayList<>();
        Iterator<Integer> itr = numbers.iterator();
        while (itr.hasNext())
        {
            Integer num = itr.next();
            Integer square = square(num);
            squares.add(square);
        }
        return squares;
    }
}
