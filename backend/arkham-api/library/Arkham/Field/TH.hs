{-# LANGUAGE TemplateHaskellQuotes #-}

{- | An entity's fields, read off the @Field@ GADT that declares them.

What can be read off an entity is declared once, as that GADT. Anything else that
needs the list -- the @FromJSON (SomeField a)@ instance that turns a name back into
a field, the properties the card builder offers -- has to agree with it, and
agreeing by hand does not last: four fields existed that nothing could ask for
because the instance had simply never been extended to mention them
(@InvestigatorCluesInPool@, @InvestigatorSearch@, @InvestigatorSideDeck@,
@LocationConcealedCards@). Nothing said so, because an unknown field name is a
parse failure and a parse failure is silence.

This is used by "Arkham.Custom.Schema" so that the builder's list, at least, is
the GADT's list by construction.

It deliberately does /not/ generate the @FromJSON@ instance. That would have to be
spliced into the entity's own module, and a top-level splice cuts a module into
declaration groups: @data instance Field Enemy@ sits hundreds of lines above
@data Enemy@ in "Arkham.Enemy.Types", so any splice between them leaves the field
instance unable to see the type it is about. The instances stay hand-written.
-}
module Arkham.Field.TH (fieldConstructors) where

import Arkham.Field (Field)
import Arkham.Prelude hiding (Type)
import Control.Monad.Fail (fail)
import Language.Haskell.TH

{- | The entity's fields, as the constructor and what reading one yields.

Matched against the data family's instances by the entity the head mentions,
rather than asked for with 'reifyInstances' -- that wants an argument count
matching the family's, which depends on how its kind signature is written.
-}
fieldConstructors :: Name -> Q [(Name, Type)]
fieldConstructors entity = do
  family <- reify ''Field
  let instances = case family of
        FamilyI _ decs -> decs
        _ -> []
  case [fieldCons | DataInstD _ _ head' _ fieldCons _ <- instances, isFor head'] of
    [fieldCons] -> pure (concatMap fromCon fieldCons)
    [] -> fail $ "No `data instance Field " <> nameBase entity <> "` to read fields from"
    _ -> fail $ "More than one `data instance Field " <> nameBase entity <> "`"
 where
  {- The entity itself, and not an entity wrapped in where it is: an asset has
  @Field Asset@ alongside @Field (DiscardedEntity Asset)@, @Field (InHandEntity
  Asset)@ and @Field (InDiscardEntity Asset)@, each with a couple of fields of its
  own. Anything looser than an exact match picks up whichever is declared first --
  which handed the builder two asset properties in place of thirty-seven. -}
  isFor ty = case strip ty of
    AppT (ConT family) argument | family == ''Field -> strip argument == ConT entity
    _ -> False

  strip = \case
    SigT t _ -> strip t
    ParensT t -> strip t
    t -> t

  {- Every field is nullary -- @Field Enemy Int@, never a function -- so what it
  yields is the last argument of its own type. -}
  fromCon = \case
    GadtC names _ ty -> [(n, result ty) | n <- names]
    RecGadtC names _ ty -> [(n, result ty) | n <- names]
    _ -> []

  result = \case
    AppT _ r -> r
    ty -> ty
