import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class SquareNumbersDeclarative
{
    public static void main (String[] args)
    {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        List<Integer> squares = firstSquares(n);
        System.out.println(squares.toString());
    }

    static List<Integer> firstSquares (int n)
    {
        IntStream numbers = IntStream.iterate(1, i -> i + 1);
        IntStream squares = numbers.map((x) -> x * x);
        return squares.boxed().limit(n).collect(Collectors.toList());
    }
}
