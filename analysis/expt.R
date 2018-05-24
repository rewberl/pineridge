library(FrF2)

doe = fac.design(nfactors=3, nlevels=c(2,2,3), factor.names=c("polymer","age","scalping"),
                 seed=20180501)
levels(doe$polymer) = c(0,1)
levels(doe$scalping) = c(0,1,2)

doe

write.table(doe, "./results/doe.txt", sep=",", quote=FALSE, row.names=T)


fac.design(nfactors=3, nlevels=2, factor.names=c("polymer","age","scalping"),
           seed=20180501)

doe2 = FrF2(nfactors=3, factor.names=list(polymer=c(0,1), age=c(1,2), scalping=c(0,1)),
            resolution=4, seed=20180501)

doe3 = FrF2(nfactors=3, factor.names=list(polymer=c(0,1), age=c(1,2), scalping=c(0,1)),
            resolution=3, seed=20180501)

write.table(doe2, "./results/doe2.txt", sep=",", quote=FALSE, row.names=T)
write.table(doe3, "./results/doe3.txt", sep=",", quote=FALSE, row.names=T)
