var hello = "hello";

class myClass {
    public myhello: string;

    constructor(public greeting: string) { }
    greet(): string {
        return "<h1>" + this.greeting + "</h1>";
    }
}
