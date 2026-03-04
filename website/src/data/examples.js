export const fallbackExamples = {
    'enum': `enum Weekday {
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday
}

fn main() {
    Weekday weekday <- Weekday::Saturday
}`,
    'if': `fn main() {
    char wasome <- showcase_if_conditionals()
}

fn showcase_if_conditionals() -> char {
    bool wasome_is_awesome <- true
 
    if (wasome_is_awesome) {
        -> '✨'
    } else {
        -> '🤥'   
    }
}`,
    'loop': `fn main() {

    // while loop
    s32 count1 <- 0

    loop (count1 < 10) {
        count1 <- count1 + 1
    }

    // for loop
    s32 sum <- 0

    loop (s32 count2 <- 0; count2 < 100; count2 <- count2 + 1) {
        sum <- sum + count2
    }
    
    // infinite loop
    s32 count3 <- 0
    
    loop {
        count3 <- count3 + 1
    }
}`,
    'operator': `fn main() {

    s32 math_showcase <- 10 * 2 + 5 - 3 / 1
    s32 num <- 10
    
    if (num < 10) {
        // less than
    }
    if (num <= 10) {
        // less than or equal
    }
    if (num == 10) {
        // equal
    }
    if (num != 10) {
        // not equal
    }
    if (num > 10) {
        // greater than
    }
    if (num >= 10) {
        // greater than or equal
    }
    if (num == 10 && true) {
        // logical and
    }
    if (num == 10 || false) {
        // logical or
    }
}`,
    'struct': `struct Point {
    s32 x
    s32 y
}

fn main() {

    Point point <- new Point { x <- 10, y <- 20 }

    s32 old_x_coordinate <- point.x
    
    point.x <- 15
}`
};

export const fallbackTree = [
    {
        name: 'single_file',
        path: 'docs/examples/single_file',
        type: 'dir',
        children: Object.keys(fallbackExamples).sort().map(key => ({
            name: `${key}.waso`,
            path: `docs/examples/single_file/${key}.waso`,
            type: 'file',
            download_url: `https://raw.githubusercontent.com/Randware/Wasome/main/docs/examples/single_file/${key}.waso`
        }))
    },
    {
        name: 'single_project',
        path: 'docs/examples/single_project',
        type: 'dir',
        children: null
    },
    {
        name: 'multi-project',
        path: 'docs/examples/multi-project',
        type: 'dir',
        children: null
    }
];
