{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.DaisyWalker where

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Source
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype DaisyWalkerMetadata = DaisyWalkerMetadata { tomeActions :: Int }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON) -- must parse to object

newtype DaisyWalkerI = DaisyWalkerI (AttrsWithMetadata DaisyWalkerMetadata)
  deriving newtype (Show, ToJSON, FromJSON)

daisyWalker :: DaisyWalkerI
daisyWalker = DaisyWalkerI $ (baseAttrs "01002" "Daisy Walker" stats [Miskatonic]) `with` DaisyWalkerMetadata 1
  where
    stats = Stats
      { health = 5
      , sanity = 9
      , willpower = 3
      , intellect = 5
      , combat = 2
      , agility = 2
      }

instance (InvestigatorRunner env) => RunMessage env DaisyWalkerI where
  runMessage msg i@(DaisyWalkerI (AttrsWithMetadata attrs@Attrs {..} metadata@DaisyWalkerMetadata {..})) = case msg of
    ActivateCardAbilityAction iid (AssetSource aid, abilityIndex, abilityType, abilityLimit) | iid == investigatorId-> do
      traits <- asks (getSet aid)
      if Tome `elem` traits && tomeActions > 0
         then case abilityType of
          FreeAbility _ -> DaisyWalkerI . (`with` metadata) <$> runMessage msg attrs
          ReactionAbility _ -> DaisyWalkerI . (`with` metadata) <$> runMessage msg attrs
          ActionAbility n actionType -> if n > 0
            then DaisyWalkerI . (`with` DaisyWalkerMetadata (tomeActions - 1)) <$> runMessage (ActivateCardAbilityAction iid (AssetSource aid, abilityIndex, (ActionAbility (n - 1) actionType), abilityLimit)) attrs
            else DaisyWalkerI . (`with` metadata) <$> runMessage msg attrs
         else DaisyWalkerI . (`with` metadata) <$> runMessage msg attrs
    ResolveToken ElderSign iid skillValue | iid == investigatorId -> do
      tomeCount <- unAssetCount <$> asks (getCount (iid, [Tome]))
      unshiftMessage
        (AddOnSuccess (Ask $ ChooseTo $ DrawCards iid tomeCount False))
      i <$ runTest skillValue
    BeginRound -> DaisyWalkerI . (`with` DaisyWalkerMetadata 1) <$> runMessage msg attrs
    _ -> DaisyWalkerI . (`with` metadata) <$> runMessage msg attrs
