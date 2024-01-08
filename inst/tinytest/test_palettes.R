# Colors =======================================================================
bw <- c("black", "white")

expect_identical(c("#FFFFC8", "#F39200", "#7D0025"), palette_color_continuous(c(1, 2, 3), NULL))
expect_identical(c("#000000", "#7F7F7F", "#FFFFFF"), palette_color_continuous(c(1, 2, 3), colorRamp(bw)))
expect_identical(c("#000000", "#7F7F7F", "#FFFFFF"), palette_color_continuous(c(0, 0.5, 1), bw))
expect_identical(c("#000000", "#7F7F7F", "#FFFFFF"), palette_color_continuous(c(1, 2, 3), bw))
expect_identical(c("#000000", "#DDDDDD", "#FFFFFF"), palette_color_continuous(c(1, NA, 3), bw))

expect_identical(c("#000000", "#1C1C1C", "#383838"), palette_color_continuous(c(1, 2, 3), bw, c(1, 2, 10)))
expect_identical(rev(c("#000000", "#7F7F7F", "#FFFFFF")), palette_color_continuous(c(1, 2, 3), rev(bw)))

expect_identical(c("#4B0055", "#4A0E5F", "#462169"), palette_color_discrete(LETTERS[1:3], NULL, LETTERS))
expect_identical(c("#000000", "#0A0A0A", "#141414"), palette_color_discrete(LETTERS[1:3], bw, LETTERS))
expect_identical(c("#000000", "#0A0A0A", "#141414"), palette_color_discrete(LETTERS[1:3], colorRampPalette(bw), LETTERS))
expect_identical(c("#4477AA", "#EE6677", "#228833"), palette_color_discrete(LETTERS[1:3], c("#4477AA", "#EE6677", "#228833"), LETTERS[1:3]))
expect_identical(c("#DDDDDD", "#DDDDDD", "#DDDDDD"), palette_color_discrete(c(NA, NA, NA), bw, LETTERS[1:3]))
expect_identical(c("#000000", "#7F7F7F", "#FFFFFF"), palette_color_discrete(LETTERS[1:3], bw, rev(LETTERS[1:3])))
expect_identical(rev(c("#000000", "#7F7F7F", "#FFFFFF")), palette_color_discrete(LETTERS[1:3], bw, rev(LETTERS[1:3]), ordered = TRUE))
