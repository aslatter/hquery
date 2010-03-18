> import Data.Maybe
> import qualified Data.Map as Map
> import Data.List
> import Data.Function


> import Codec.Archive.Tar as Tar
> import Codec.Archive.Tar.Entry as Tar

> import Data.ByteString.Lazy (ByteString)
> import qualified Data.ByteString.Lazy as BS

> import qualified Data.ByteString.Lazy.UTF8 as UTF8

> import System.IO
> import System.Environment

> import Distribution.Text
> import Text.PrettyPrint
> import Distribution.Package
> import Distribution.PackageDescription
> import Distribution.PackageDescription.Parse
> import Distribution.Version

And now the 'main' method.

The first argument is the name of the package you're checking dependencies for, and the second argument is the path to the Hackage index tar-file (for me this is ~/.cabal/packages/hackage.haskell.org/00-index.tar).

> main :: IO ()
> main = do
>  [target,tarball] <- getArgs
>  withFile tarball ReadMode $ \h -> do
>        contents <- BS.hGetContents h
>        let matches = matchesFromIndex contents (== target)
>        sequence_ $ map (print . printRes) matches 

And then we have the function which, given the contents of the tar-file as a ByteString, returns back the list of PackageIds which depend on the indicated package.

> -- matchesFromIndex :: ByteString -> (String -> Bool) -> [PackageId]
> matchesFromIndex index p =

>  let entries = Tar.read index
>      cabalFiles = map UTF8.toString $ findCabalEntries entries
>      parseResults = map parsePackageDescription cabalFiles
>      gPckgDiscs = okayOnly parseResults
>      matches = filter (filterDep p) $ nubPkg $ filter (match p) gPckgDiscs

>  in map (format p) matches


> printRes (a, b) = disp a <+> parens (disp (head b))

> format :: (String -> Bool) -> GenericPackageDescription -> (PackageId, [Dependency])
> format p pkg = (packageId pkg, findDep p pkg)

> nubPkg :: Package pkg => [pkg] -> [pkg]
> nubPkg xs = go Map.empty xs
>  where
>    go m [] = Map.elems m
>    go m (p:ps) | Map.member (packageName p) m
>               = let Just p' = Map.lookup (packageName p) m
>                     m' = if (packageVersion p > packageVersion p')
>                           then Map.insert (packageName p) p m
>                           else m
>                 in go m' ps
>                 | otherwise = go (Map.insert (packageName p) p m) ps

> okayOnly :: [ParseResult a] -> [a]
> okayOnly = mapMaybe fromOkay
>  where fromOkay (ParseOk _ a) = Just a
>        fromOkay _ = Nothing

> -- Does this package have a dependency which matches our
> -- query?
> match ::  (String -> Bool) -> GenericPackageDescription -> Bool
> match p pkg = any (matchDep p) (gPckgDeps pkg)

> findDep :: (String -> Bool) -> GenericPackageDescription -> [Dependency]
> findDep p pkg = filter (matchDep p) (gPckgDeps pkg)

> glomDeps :: [Dependency] -> Dependency
> glomDeps [] = error "glomDeps: requires non-null list"
> glomDeps (d:[]) = d
> glomDeps ((Dependency p v):xs) = Dependency p $ foldr UnionVersionRanges v $ map f xs
>  where f (Dependency _ v) = v

> filterDep :: (String -> Bool) -> GenericPackageDescription -> Bool
> filterDep p = versionTest . glomDeps . findDep p

> versionTest (Dependency _ ver)
>                 | withinRange ver3 ver = (not $ withinRange ver2 ver) &&
>                                          (withinRange ver4 ver)
> versionTest _ = False 

> ver2 = Version [2,1,0,1] []
> ver3 = Version [3,1] []
> ver4 = Version [4,0,0] []


> -- Does this dependency match our query?
> matchDep :: (String -> Bool) -> Dependency -> Bool
> matchDep p (Dependency (PackageName name) _) = p name

There's a bit of black-magic going on here - I don't entirely understand the structure of the new 'library' and 'executable' sections of the .cabal file, but I scrape everything out which has the right type.

> gPckgDeps :: GenericPackageDescription -> [Dependency]
> gPckgDeps pkg = normalDeps ++ libDeps ++ execDeps
>  where
>   normalDeps = buildDepends $ packageDescription pkg

>   libDeps = case condLibrary pkg of
>               Nothing -> []
>               Just cndTree -> depsFromCndTree exLibDeps cndTree

>   execDeps = concatMap (depsFromCndTree exExecDeps . snd)
>                        (condExecutables pkg)

>   exLibDeps = pkgconfigDepends . libBuildInfo
>   exExecDeps = pkgconfigDepends . buildInfo

> depsFromCndTree f tree =
>   let x = condTreeData tree
    
>       parts = condTreeComponents tree
>       mdlTrees = map mdl parts
>       thrdTrees = mapMaybe thrd parts

>       trees = mdlTrees ++ thrdTrees


>   in f x ++
>      condTreeConstraints tree ++
>      concatMap (depsFromCndTree f) trees

>  where mdl (_,x,_) = x
>        thrd (_,_,x) = x

And this is the bit which takes the decoded tar-file and returns back the entries which look like they could be .cabal files.

> findCabalEntries :: Tar.Entries -> [ByteString]
> findCabalEntries entries = Tar.foldEntries go [] (const []) entries

>  where go :: Tar.Entry -> [ByteString] -> [ByteString]
>        go x xs = case Tar.entryContent x of
>                    NormalFile bs _
>                        | isBoringName (entryFilePath x) -> xs
>                        | otherwise -> bs:xs
>                    _ -> xs
>

>        entryFilePath = Tar.fromTarPath . Tar.entryTarPath
>        isBoringName = not . isSuffixOf ".cabal"
