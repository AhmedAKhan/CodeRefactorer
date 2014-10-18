import static org.junit.Assert.*;
import org.junit.BeforeClass;
import org.junit.Test;
import se2s03.BadCode;
public class BadCodeTest {
    private static BadCode tester;
    @BeforeClass
    public static void setUp(){
        tester = new BadCode();
    }
    @Test
    public void casesTest() {
        assertEquals("badCode(40, -18, 4, 15, 13, -34) must be 47.84241549817206", 47.84241549817206, tester.badCode(40, -18, 4, 15, 13, -34), 1.0e-2);
        assertEquals("badCode(-32, 14, 41, 17, -21, -30) must be 568.842415498172", 568.842415498172, tester.badCode(-32, 14, 41, 17, -21, -30), 1.0e-2);
        assertEquals("badCode(9, 28, -38, 23, 37, 6) must be 1180.8424154981722", 1180.8424154981722, tester.badCode(9, 28, -38, 23, 37, 6), 1.0e-2);
        assertEquals("badCode(23, 34, 48, -38, -45, -23) must be 2207.842415498172", 2207.842415498172, tester.badCode(23, 34, 48, -38, -45, -23), 1.0e-2);
        assertEquals("badCode(40, -25, 38, 22, -35, 44) must be 1407.8424154981722", 1407.8424154981722, tester.badCode(40, -25, 38, 22, -35, 44), 1.0e-2);
        assertEquals("badCode(9, 42, -15, 16, -32, 47) must be 1175.8424154981722", 1175.8424154981722, tester.badCode(9, 42, -15, 16, -32, 47), 1.0e-2);
        assertEquals("badCode(-15, -48, 50, 40, -4, 40) must be 137.84241549817204", 137.84241549817204, tester.badCode(-15, -48, 50, 40, -4, 40), 1.0e-2);
        assertEquals("badCode(44, -50, -16, -47, -17, 23) must be 320.84241549817204", 320.84241549817204, tester.badCode(44, -50, -16, -47, -17, 23), 1.0e-2);
        assertEquals("badCode(-7, -37, 10, 34, -37, -48) must be 1486.8424154981722", 1486.8424154981722, tester.badCode(-7, -37, 10, 34, -37, -48), 1.0e-2);
        assertEquals("badCode(-18, -33, 36, 13, -32, -32) must be 1174.8424154981722", 1174.8424154981722, tester.badCode(-18, -33, 36, 13, -32, -32), 1.0e-2);
        assertEquals("badCode(3, -38, 26, -38, 3, -45) must be -24.15758450182794", -24.15758450182794, tester.badCode(3, -38, 26, -38, 3, -45), 1.0e-2);
        assertEquals("badCode(2, -5, 47, -30, 22, -25) must be 416.84241549817204", 416.84241549817204, tester.badCode(2, -5, 47, -30, 22, -25), 1.0e-2);
        assertEquals("badCode(42, -25, 16, -25, 8, -22) must be -15.157584501827941", -15.157584501827941, tester.badCode(42, -25, 16, -25, 8, -22), 1.0e-2);
        assertEquals("badCode(-11, -38, -28, 12, -15, 7) must be 275.84241549817204", 275.84241549817204, tester.badCode(-11, -38, -28, 12, -15, 7), 1.0e-2);
        assertEquals("badCode(32, -5, -15, -16, 18, -8) must be 197.84241549817204", 197.84241549817204, tester.badCode(32, -5, -15, -16, 18, -8), 1.0e-2);
        assertEquals("badCode(32, 39, 31, -23, 39, -45) must be 1319.8424154981722", 1319.8424154981722, tester.badCode(32, 39, 31, -23, 39, -45), 1.0e-2);
        assertEquals("badCode(-26, 8, -3, 31, 42, 45) must be 1664.8424154981722", 1664.8424154981722, tester.badCode(-26, 8, -3, 31, 42, 45), 1.0e-2);
        assertEquals("badCode(-12, -38, -6, -47, 28, -25) must be 653.842415498172", 653.842415498172, tester.badCode(-12, -38, -6, -47, 28, -25), 1.0e-2);
        assertEquals("badCode(-13, -40, 45, 19, 48, 17) must be 2187.842415498172", 2187.842415498172, tester.badCode(-13, -40, 45, 19, 48, 17), 1.0e-2);
        assertEquals("badCode(24, -17, 28, -30, 30, 4) must be 788.842415498172", 788.842415498172, tester.badCode(24, -17, 28, -30, 30, 4), 1.0e-2);
        assertEquals("badCode(-18, -13, 28, -20, -23, 16) must be 683.842415498172", 683.842415498172, tester.badCode(-18, -13, 28, -20, -23, 16), 1.0e-2);
        assertEquals("badCode(-18, 35, -23, 42, -14, 31) must be 278.84241549817204", 278.84241549817204, tester.badCode(-18, 35, -23, 42, -14, 31), 1.0e-2);
        assertEquals("badCode(-43, -17, -38, 15, -30, 36) must be 1061.8424154981722", 1061.8424154981722, tester.badCode(-43, -17, -38, 15, -30, 36), 1.0e-2);
        assertEquals("badCode(-18, -50, -45, 48, 8, -29) must be -23.15758450182794", -23.15758450182794, tester.badCode(-18, -50, -45, 48, 8, -29), 1.0e-2);
        assertEquals("badCode(47, -18, 43, 28, 10, 24) must be 80.84241549817206", 80.84241549817206, tester.badCode(47, -18, 43, 28, 10, 24), 1.0e-2);
        assertEquals("badCode(-38, 24, 7, 22, -20, 9) must be 534.842415498172", 534.842415498172, tester.badCode(-38, 24, 7, 22, -20, 9), 1.0e-2);
        assertEquals("badCode(8, 36, 29, -44, 45, 24) must be 1890.8424154981722", 1890.8424154981722, tester.badCode(8, 36, 29, -44, 45, 24), 1.0e-2);
        assertEquals("badCode(-34, 29, 33, 44, 36, 29) must be 1248.8424154981722", 1248.8424154981722, tester.badCode(-34, 29, 33, 44, 36, 29), 1.0e-2);
        assertEquals("badCode(47, 7, 46, 11, 41, -39) must be 1477.8424154981722", 1477.8424154981722, tester.badCode(47, 7, 46, 11, 41, -39), 1.0e-2);
        assertEquals("badCode(50, -50, -22, -15, 31, -5) must be 760.842415498172", 760.842415498172, tester.badCode(50, -50, -22, -15, 31, -5), 1.0e-2);
        assertEquals("badCode(-28, 32, -49, 15, 26, -10) must be 541.842415498172", 541.842415498172, tester.badCode(-28, 32, -49, 15, 26, -10), 1.0e-2);
        assertEquals("badCode(21, 20, 10, 7, 29, 24) must be 738.842415498172", 738.842415498172, tester.badCode(21, 20, 10, 7, 29, 24), 1.0e-2);
        assertEquals("badCode(-31, 39, -34, 7, -2, -48) must be -38.15758450182794", -38.15758450182794, tester.badCode(-31, 39, -34, 7, -2, -48), 1.0e-2);
        assertEquals("badCode(-26, -5, -4, -17, -46, 48) must be 2370.842415498172", 2370.842415498172, tester.badCode(-26, -5, -4, -17, -46, 48), 1.0e-2);
        assertEquals("badCode(8, -43, 43, 29, 8, 25) must be 92.84241549817206", 92.84241549817206, tester.badCode(8, -43, 43, 29, 8, 25), 1.0e-2);
        assertEquals("badCode(22, 50, 21, 11, -8, 18) must be 113.84241549817206", 113.84241549817206, tester.badCode(22, 50, 21, 11, -8, 18), 1.0e-2);
        assertEquals("badCode(19, 10, -38, -38, -8, -11) must be 28.84241549817206", 28.84241549817206, tester.badCode(19, 10, -38, -38, -8, -11), 1.0e-2);
        assertEquals("badCode(-11, -7, 7, 44, -3, 7) must be 46.84241549817206", 46.84241549817206, tester.badCode(-11, -7, 7, 44, -3, 7), 1.0e-2);
        assertEquals("badCode(46, 48, -45, 20, -29, -46) must be 820.842415498172", 820.842415498172, tester.badCode(46, 48, -45, 20, -29, -46), 1.0e-2);
        assertEquals("badCode(20, -15, 7, -12, -34, -14) must be 1265.8424154981722", 1265.8424154981722, tester.badCode(20, -15, 7, -12, -34, -14), 1.0e-2);
        assertEquals("badCode(-34, 49, 25, -25, 18, 34) must be 345.84241549817204", 345.84241549817204, tester.badCode(-34, 49, 25, -25, 18, 34), 1.0e-2);
        assertEquals("badCode(15, -45, 4, 9, 33, 11) must be 957.842415498172", 957.842415498172, tester.badCode(15, -45, 4, 9, 33, 11), 1.0e-2);
        assertEquals("badCode(17, 14, 26, 42, -21, -20) must be 514.842415498172", 514.842415498172, tester.badCode(17, 14, 26, 42, -21, -20), 1.0e-2);
        assertEquals("badCode(-28, -15, -13, 26, 37, 12) must be 1248.8424154981722", 1248.8424154981722, tester.badCode(-28, -15, -13, 26, 37, 12), 1.0e-2);
        assertEquals("badCode(47, -40, -21, 10, 11, -7) must be 2.842415498172059", 2.842415498172059, tester.badCode(47, -40, -21, 10, 11, -7), 1.0e-2);
        assertEquals("badCode(-30, 8, 8, 48, -42, 28) must be 1998.8424154981722", 1998.8424154981722, tester.badCode(-30, 8, 8, 48, -42, 28), 1.0e-2);
        assertEquals("badCode(16, -34, -26, -3, -46, -46) must be 2212.842415498172", 2212.842415498172, tester.badCode(16, -34, -26, -3, -46, -46), 1.0e-2);
        assertEquals("badCode(22, -11, -26, -20, -10, -34) must be 58.84241549817206", 58.84241549817206, tester.badCode(22, -11, -26, -20, -10, -34), 1.0e-2);
        assertEquals("badCode(-21, -39, 7, 8, -15, 10) must be 323.84241549817204", 323.84241549817204, tester.badCode(-21, -39, 7, 8, -15, 10), 1.0e-2);
        assertEquals("badCode(-19, 32, -6, 5, -45, 36) must be 2254.842415498172", 2254.842415498172, tester.badCode(-19, 32, -6, 5, -45, 36), 1.0e-2);
        assertEquals("badCode(21, -41, -39, -50, 30, 14) must be 734.842415498172", 734.842415498172, tester.badCode(21, -41, -39, -50, 30, 14), 1.0e-2);
        assertEquals("badCode(-40, -15, -7, -27, 34, 46) must be 1099.8424154981722", 1099.8424154981722, tester.badCode(-40, -15, -7, -27, 34, 46), 1.0e-2);
        assertEquals("badCode(-14, 15, 40, -17, -50, 20) must be 2774.842415498172", 2774.842415498172, tester.badCode(-14, 15, 40, -17, -50, 20), 1.0e-2);
        assertEquals("badCode(8, 43, 49, -21, -48, -50) must be 2487.842415498172", 2487.842415498172, tester.badCode(8, 43, 49, -21, -48, -50), 1.0e-2);
        assertEquals("badCode(22, -47, 20, 13, 46, -16) must be 1914.8424154981722", 1914.8424154981722, tester.badCode(22, -47, 20, 13, 46, -16), 1.0e-2);
        assertEquals("badCode(-48, -31, 13, -28, 41, 26) must be 1604.8424154981722", 1604.8424154981722, tester.badCode(-48, -31, 13, -28, 41, 26), 1.0e-2);
        assertEquals("badCode(-21, 11, -50, 10, 47, -19) must be 1973.8424154981722", 1973.8424154981722, tester.badCode(-21, 11, -50, 10, 47, -19), 1.0e-2);
        assertEquals("badCode(14, 46, 39, 8, -40, -45) must be 1740.8424154981722", 1740.8424154981722, tester.badCode(14, 46, 39, 8, -40, -45), 1.0e-2);
        assertEquals("badCode(-39, 13, 28, -35, 19, 17) must be 369.84241549817204", 369.84241549817204, tester.badCode(-39, 13, 28, -35, 19, 17), 1.0e-2);
        assertEquals("badCode(-43, -35, -39, -46, -17, 20) must be 381.84241549817204", 381.84241549817204, tester.badCode(-43, -35, -39, -46, -17, 20), 1.0e-2);
        assertEquals("badCode(-19, 10, -43, 28, 39, 36) must be 1377.8424154981722", 1377.8424154981722, tester.badCode(-19, 10, -43, 28, 39, 36), 1.0e-2);
        assertEquals("badCode(40, -25, 41, -38, -42, -16) must be 1917.8424154981722", 1917.8424154981722, tester.badCode(40, -25, 41, -38, -42, -16), 1.0e-2);
        assertEquals("badCode(-15, 47, -35, -22, -39, -33) must be 1624.8424154981722", 1624.8424154981722, tester.badCode(-15, 47, -35, -22, -39, -33), 1.0e-2);
        assertEquals("badCode(44, -26, 6, -20, -30, -30) must be 952.842415498172", 952.842415498172, tester.badCode(44, -26, 6, -20, -30, -30), 1.0e-2);
        assertEquals("badCode(-24, -31, -44, -37, 2, -11) must be -34.15758450182794", -34.15758450182794, tester.badCode(-24, -31, -44, -37, 2, -11), 1.0e-2);
        assertEquals("badCode(42, -2, 46, 16, -9, 42) must be 163.84241549817204", 163.84241549817204, tester.badCode(42, -2, 46, 16, -9, 42), 1.0e-2);
        assertEquals("badCode(-18, -28, 41, 22, -31, 11) must be 1155.8424154981722", 1155.8424154981722, tester.badCode(-18, -28, 41, 22, -31, 11), 1.0e-2);
        assertEquals("badCode(-36, -2, -2, -38, 10, -45) must be 49.84241549817206", 49.84241549817206, tester.badCode(-36, -2, -2, -38, 10, -45), 1.0e-2);
        assertEquals("badCode(29, 39, 40, 4, -14, -6) must be 257.84241549817204", 257.84241549817204, tester.badCode(29, 39, 40, 4, -14, -6), 1.0e-2);
        assertEquals("badCode(10, 9, 42, 33, 5, 46) must be 83.84241549817206", 83.84241549817206, tester.badCode(10, 9, 42, 33, 5, 46), 1.0e-2);
        assertEquals("badCode(-37, -4, 32, 21, -25, -26) must be 768.842415498172", 768.842415498172, tester.badCode(-37, -4, 32, 21, -25, -26), 1.0e-2);
        assertEquals("badCode(32, -50, 2, 37, 31, -38) must be 769.842415498172", 769.842415498172, tester.badCode(32, -50, 2, 37, 31, -38), 1.0e-2);
        assertEquals("badCode(-46, 11, 35, -3, -12, 18) must be 291.84241549817204", 291.84241549817204, tester.badCode(-46, 11, 35, -3, -12, 18), 1.0e-2);
        assertEquals("badCode(30, 21, -8, -31, 23, -12) must be 387.84241549817204", 387.84241549817204, tester.badCode(30, 21, -8, -31, 23, -12), 1.0e-2);
        assertEquals("badCode(-38, 10, 6, -8, 36, -43) must be 1153.8424154981722", 1153.8424154981722, tester.badCode(-38, 10, 6, -8, 36, -43), 1.0e-2);
        assertEquals("badCode(-20, 48, -16, -27, -5, -5) must be 44.84241549817206", 44.84241549817206, tester.badCode(-20, 48, -16, -27, -5, -5), 1.0e-2);
        assertEquals("badCode(-8, 27, -20, 36, -29, 30) must be 975.842415498172", 975.842415498172, tester.badCode(-8, 27, -20, 36, -29, 30), 1.0e-2);
        assertEquals("badCode(12, -31, 45, -31, 11, 16) must be 126.84241549817206", 126.84241549817206, tester.badCode(12, -31, 45, -31, 11, 16), 1.0e-2);
        assertEquals("badCode(-39, -32, -44, 12, -48, 14) must be 2505.842415498172", 2505.842415498172, tester.badCode(-39, -32, -44, 12, -48, 14), 1.0e-2);
        assertEquals("badCode(-23, 49, -47, -35, 50, 9) must be 2285.842415498172", 2285.842415498172, tester.badCode(-23, 49, -47, -35, 50, 9), 1.0e-2);
        assertEquals("badCode(29, -24, -12, -33, 32, -16) must be 839.842415498172", 839.842415498172, tester.badCode(29, -24, -12, -33, 32, -16), 1.0e-2);
        assertEquals("badCode(36, -46, 33, -3, 27, 44) must be 662.842415498172", 662.842415498172, tester.badCode(36, -46, 33, -3, 27, 44), 1.0e-2);
        assertEquals("badCode(-11, -34, 5, 41, 8, -3) must be 45.84241549817206", 45.84241549817206, tester.badCode(-11, -34, 5, 41, 8, -3), 1.0e-2);
        assertEquals("badCode(4, 6, -17, -27, 31, 4) must be 820.842415498172", 820.842415498172, tester.badCode(4, 6, -17, -27, 31, 4), 1.0e-2);
        assertEquals("badCode(-5, -39, -41, -18, 14, 10) must be 114.84241549817206", 114.84241549817206, tester.badCode(-5, -39, -41, -18, 14, 10), 1.0e-2);
        assertEquals("badCode(3, 38, -27, 37, 45, 20) must be 1835.8424154981722", 1835.8424154981722, tester.badCode(3, 38, -27, 37, 45, 20), 1.0e-2);
        assertEquals("badCode(-17, 9, 26, 9, 9, -25) must be 63.84241549817206", 63.84241549817206, tester.badCode(-17, 9, 26, 9, 9, -25), 1.0e-2);
        assertEquals("badCode(-41, -7, -48, 30, -5, 6) must be 44.84241549817206", 44.84241549817206, tester.badCode(-41, -7, -48, 30, -5, 6), 1.0e-2);
        assertEquals("badCode(42, -41, 32, -29, -25, -48) must be 667.842415498172", 667.842415498172, tester.badCode(42, -41, 32, -29, -25, -48), 1.0e-2);
        assertEquals("badCode(6, 22, 13, -26, 19, 49) must be 341.84241549817204", 341.84241549817204, tester.badCode(6, 22, 13, -26, 19, 49), 1.0e-2);
        assertEquals("badCode(-43, -28, -41, -26, 30, 10) must be 792.842415498172", 792.842415498172, tester.badCode(-43, -28, -41, -26, 30, 10), 1.0e-2);
        assertEquals("badCode(-32, 16, -14, -46, -40, -8) must be 1770.8424154981722", 1770.8424154981722, tester.badCode(-32, 16, -14, -46, -40, -8), 1.0e-2);
        assertEquals("badCode(11, -35, -30, 18, -25, -31) must be 653.842415498172", 653.842415498172, tester.badCode(11, -35, -30, 18, -25, -31), 1.0e-2);
        assertEquals("badCode(18, -33, -10, 46, 32, 25) must be 893.842415498172", 893.842415498172, tester.badCode(18, -33, -10, 46, 32, 25), 1.0e-2);
        assertEquals("badCode(39, -33, -24, 2, 22, -37) must be 296.84241549817204", 296.84241549817204, tester.badCode(39, -33, -24, 2, 22, -37), 1.0e-2);
        assertEquals("badCode(43, 35, -22, 15, -6, 10) must be 5.842415498172059", 5.842415498172059, tester.badCode(43, 35, -22, 15, -6, 10), 1.0e-2);
        assertEquals("badCode(-38, 33, -16, -13, 20, -36) must be 306.84241549817204", 306.84241549817204, tester.badCode(-38, 33, -16, -13, 20, -36), 1.0e-2);
        assertEquals("badCode(7, 38, 41, 23, -42, 2) must be 1968.8424154981722", 1968.8424154981722, tester.badCode(7, 38, 41, 23, -42, 2), 1.0e-2);
        assertEquals("badCode(-48, 5, 22, 2, -48, 22) must be 2588.842415498172", 2588.842415498172, tester.badCode(-48, 5, 22, 2, -48, 22), 1.0e-2);
        assertEquals("badCode(39, 21, -29, -26, -7, -46) must be -36.15758450182794", -36.15758450182794, tester.badCode(39, 21, -29, -26, -7, -46), 1.0e-2);
    }
}