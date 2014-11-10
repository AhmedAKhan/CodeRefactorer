package se2s03;
public class BadCode {
    public int x1;
    public int x2;
    public int x3;
    public double x4;
    public double x5;
    public double x6;
    
    public double badCode(int var1, int var2, int var3, int var4, int var5, int var6) {
        x1 = -4;
        x2 = -4;
        x3 = -3;
        x4 = 5.509789193905533;
        x5 = 9.842415498172059;
        x6 = 24.176045683662835;
        
        double endValue = 0.0;
        
        endValue += func1(var1);
        endValue += func2(var2);
        endValue += func3(var3);
        endValue += func4(var4);
        endValue += func5(var5);
        endValue += func6(var6);
        
        return endValue;
    }
    
    private int func1(int inputParam) {
        double x5 = this.x4;
        int x1 = this.x1;
        int x2 = this.x2;
        double x4 = this.x5;
        int v998 = this.x3;
        int v143 = 11;
        
        x2 = v143 - x2;
        
        x1 = v143 - inputParam;
        
        x4 = x2 - x1;
        
        if (14.176045683662835 < this.x6) {
            v998 = x1 - x2;
        }
        else {
            x4 = x4 * (x5 + this.x4);
            this.x4 = Math.sin(this.x4);
        }
        while (-3 == x2) {
            int throwsError = 0 / 0;
        }
        
        x5 = -(x4);
        while (69 < this.x2) {
            int throwsError = 0 / 0;
        }
        
        v143 = v998 + x2;
        
        return v143;
    }
    
    private double func2(int inputParam) {
        int v622 = this.x1;
        int x3 = this.x2;
        double x5 = this.x4;
        int v624 = this.x3;
        int x2 = -21;
        double x6 = this.x5;
        
        if (-0.15758450182794093 < this.x5) {
            x3 = inputParam + v624;
        }
        else {
            this.x3 = ((this.x3 + v624) + x3) - (this.x3 * this.x3);
            this.x5 = -(x5);
            return x3 * v624;
        }
        
        if (-14 < this.x1) {
            v624 = v622 - x3;
        }
        else {
            this.x1 = v622 * x2;
            x2 = x3 + this.x1;
        }
        while (26.332916914240784 < x5) {
            int throwsError = 0 / 0;
        }
        
        x2 = v622 - v624;
        
        x5 = x3 * inputParam;
        
        v622 = v624 - inputParam;
        
        x6 = x6 + this.x2;
        
        return x6;
    }
    
    private int func3(int inputParam) {
        int v954 = this.x1;
        int v529 = this.x2;
        double x6 = this.x4;
        double x5 = this.x5;
        int x2 = this.x3;
        int v235 = 3;
        
        v235 = v954 + v235;
        
        x5 = inputParam + v529;
        
        v954 = this.x2 - v954;
        while (80.61323734548725 < this.x6) {
            int throwsError = 0 / 0;
        }
        
        x6 = Math.cos(inputParam);
        
        v529 = x2 + v954;
        if (-27 == v235) {
            int throwsError = 0 / 0;
        }
        
        if (-10 < v954) {
            x2 = inputParam + x2;
        }
        else {
            this.x5 = this.x4 * Math.sin(this.x5);
            v954 = (this.x2 + this.x2) - this.x1;
            return x2 * v235;
        }
        
        return x2;
    }
    
    private int func4(int inputParam) {
        int v105 = this.x1;
        int x3 = this.x2;
        int v263 = this.x3;
        double x4 = this.x4;
        int v529 = -16;
        int v800 = 18;
        
        x4 = x3 * inputParam;
        
        v800 = v529 - v105;
        
        x3 = x3 - inputParam;
        
        v105 = inputParam - v800;
        
        v529 = x3 - inputParam;
        
        v263 = v800 - v263;
        
        return v263;
    }
    
    private double func5(int inputParam) {
        int x1 = this.x1;
        int v759 = this.x2;
        int v998 = this.x3;
        double x5 = this.x4;
        int v540 = 8;
        double x6 = this.x5;
        
        v998 = inputParam + x1;
        
        v540 = v998 - v759;
        
        v759 = v540 + v759;
        if (103.70686505088534 < this.x5) {
            int throwsError = 0 / 0;
        }
        
        x1 = v759 * inputParam;
        
        x5 = v540 + inputParam;
        while (-80 == this.x2) {
            int throwsError = 0 / 0;
        }
        
        x6 = v540 * v998;
        
        return x6;
    }
    
    private int func6(int inputParam) {
        int v763 = this.x1;
        int v292 = this.x2;
        int x3 = this.x3;
        double x6 = this.x4;
        int v504 = 18;
        int v759 = -29;
        
        v504 = inputParam + v292;
        
        v763 = v759 - x3;
        
        x6 = this.x3 + x6;
        
        v759 = v759 - v763;
        
        x3 = v759 - this.x1;
        
        v292 = inputParam + v292;
        
        return v292;
    }
}