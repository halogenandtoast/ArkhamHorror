{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Investigator.Cards.RolandBanks
  ( RolandBanks(..)
  , rolandBanks
  )
where

import Arkham.Import

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype RolandBanks = RolandBanks Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env RolandBanks where
  getModifiersFor source target (RolandBanks attrs) =
    getModifiersFor source target attrs

rolandBanks :: RolandBanks
rolandBanks = RolandBanks
  $ baseAttrs "01001" "Roland Banks" Guardian stats [Agency, Detective]
 where
  stats = Stats
    { health = 9
    , sanity = 5
    , willpower = 3
    , intellect = 3
    , combat = 4
    , agility = 2
    }

ability :: Attrs -> Ability
ability attrs = base { abilityLimit = PlayerLimit PerRound 1 }
 where
  base =
    mkAbility (toSource attrs) 1 (ReactionAbility (WhenEnemyDefeated You) Free)

instance ActionRunner env => HasActions env RolandBanks where
  getActions iid (WhenEnemyDefeated You) (RolandBanks a@Attrs {..})
    | iid == investigatorId = do
      let ability' = (investigatorId, ability a)
      clueCount' <- unClueCount <$> getCount investigatorLocation
      pure [ uncurry ActivateCardAbilityAction ability' | clueCount' > 0 ]
  getActions _ _ _ = pure []

instance HasCount ClueCount env LocationId => HasTokenValue env RolandBanks where
  getTokenValue (RolandBanks attrs) iid ElderSign
    | iid == investigatorId attrs = do
      locationClueCount <- unClueCount <$> getCount (investigatorLocation attrs)
      pure $ TokenValue ElderSign (PositiveModifier locationClueCount)
  getTokenValue (RolandBanks attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env RolandBanks where
  runMessage msg rb@(RolandBanks attrs@Attrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 | iid == investigatorId ->
      rb <$ unshiftMessage
        (DiscoverCluesAtLocation investigatorId investigatorLocation 1)
    _ -> RolandBanks <$> runMessage msg attrs
