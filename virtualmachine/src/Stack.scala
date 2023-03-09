class Stack[A](size: Int) {
    private var stack: List[A] = List()

    def push(value: A): Attempt[Unit] = {
        if (stack.length == size) { return Attempt.failure("Stack Overflow") }

        stack = value :: stack
        return Attempt.success(())
    }

    def pop(): Attempt[A] = {
        if (stack.length == 0) { return Attempt.failure("Stack Underflow") }

        val value = stack.head
        stack = stack.tail
        return Attempt.success(value)
    }

    def pushmany(value: List[A]): Attempt[Unit] = {
        // Push a list of values onto the stack
        if (stack.length + value.length > size) {
            return Attempt.failure("Stack Overflow")
        }

        stack = value ++ stack
        return Attempt.success(())
    }

    def popmany(count: Int): Attempt[List[A]] = {
        // Pop count elements off the stack and as a list
        if (stack.length < count) { return Attempt.failure("Stack Underflow") }

        val (values, rest) = stack.splitAt(count)
        stack = rest
        return Attempt.success(values)
    }

    def peek(): Attempt[A] = {
        if (stack.length == 0) { return Attempt.failure("Stack Underflow") }

        return Attempt.success(stack.head)
    }

    def isEmpty: Boolean = { return stack.length == 0 }

    def data(): List[A] = { return stack }
}
