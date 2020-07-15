### Comparing a and b

library(tidyverse)

p1=ggplot() +
  geom_segment(aes(x = 1, xend = 9, y = 10.32 + .704, yend = 10.32 + .704*9)) +
  geom_segment(aes(x = 1, xend = 9, y = 8.32 + .704, yend = 8.32 + .704*9)) +
  geom_segment(aes(x = 1, xend = 9, y = 5.32 + .704, yend = 5.32 + .704*9)) +
  xlim(1, 9) +
  ggtitle("Different a,  Same b")+
  ylab("Y") +
  xlab("X") + 
  annotate("text", x = 8, y = 15.2, label = "y' = 10.32 + 0.704b")+ 
  annotate("text", x = 8, y = 13.2, label = "y' = 8.32 + 0.704b") +
  annotate("text", x = 8, y = 10.2, label = "y' = 5.32 + 0.704b")



p2=ggplot() +
  geom_segment(aes(x = 1, xend = 9, y = 10.32 + .704, yend = 10.32 + .704*9)) +
  geom_segment(aes(x = 1, xend = 9, y = 8.32 + .704, yend = 8.32 + .704*9)) +
  geom_segment(aes(x = 1, xend = 9, y = 5.32 + .704, yend = 5.32 + .704*9)) +
  geom_segment(aes(x = 0, xend = 1, y = 10.32, yend = 10.32+ .704), color="red") +
  geom_segment(aes(x = 0, xend = 1, y = 8.32, yend = 8.32 + .704), color="red") +
  geom_segment(aes(x = 0, xend = 1, y = 5.32, yend = 5.32 + .704, color="red")) +
  xlim(-1,9) + 
  geom_vline(xintercept=0, color='red', lty=2) +
  theme(legend.position = "none")+
  ggtitle("Different a,  Same b")+
  ylab("Y") +
  xlab("X")+ 
  annotate("text", x = 8, y = 15.2, label = "y' = 10.32 + 0.704b")+ 
  annotate("text", x = 8, y = 13.2, label = "y' = 8.32 + 0.704b") +
  annotate("text", x = 8, y = 10.2, label = "y' = 5.32 + 0.704b")




p3=ggplot() +
  geom_segment(aes(x = 1, xend = 9, y = 8.32 + .350, yend = 8.32 + .350*9)) +
  geom_segment(aes(x = 1, xend = 9, y = 8.32 + .704, yend = 8.32 + .704*9)) +
  geom_segment(aes(x = 1, xend = 9, y = 8.32 + -.622, yend = 8.32 + -.622*9)) +
  xlim(1, 9)+
  ggtitle("Same a,  Different b") +
  ylab("Y") +
  xlab("X")+ 
  annotate("text", x = 8, y = 15, label = "y' = 8.32 + 0.350b")+ 
  annotate("text", x = 8, y = 10.5, label = "y' = 8.32 + 0.704b") +
  annotate("text", x = 8, y = 4.5, label = "y' = 8.32 + -0.622b")




p4=ggplot() +
  geom_segment(aes(x = 1, xend = 9, y = 8.32 + .350, yend = 8.32 + .350*9)) +
  geom_segment(aes(x = 1, xend = 9, y = 8.32 + .704, yend = 8.32 + .704*9)) +
  geom_segment(aes(x = 1, xend = 9, y = 8.32 + -.622, yend = 8.32 + -.622*9)) +
  geom_segment(aes(x = 0, xend = 1, y = 8.32, yend = 8.32 + .350), color="red") +
  geom_segment(aes(x = 0, xend = 1, y = 8.32, yend = 8.32 + .704), color="red") +
  geom_segment(aes(x = 0, xend = 1, y = 8.32, yend = 8.32 + -.622, color="red")) +
  xlim(-1,9) + 
  geom_vline(xintercept=0, color='red', lty=2) +
  theme(legend.position = "none")+
  ggtitle("Same a,  Different b")+
  ylab("Y") +
  xlab("X")+ 
  annotate("text", x = 8, y = 15, label = "y' = 8.32 + 0.350b")+ 
  annotate("text", x = 8, y = 10.5, label = "y' = 8.32 + 0.704b") +
  annotate("text", x = 8, y = 4.5, label = "y' = 8.32 + -0.622b")


library(gridExtra)
grid.arrange(p1,p2,p3,p4,nrow=2)

