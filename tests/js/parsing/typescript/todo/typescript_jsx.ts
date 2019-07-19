interface ArrayConstructor {

    // ambiguity with JSX!
    <T>(...items: T[]): T[];
}
