{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.RolandBanks where

import Arkham.Import

import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait

newtype RolandBanks = RolandBanks Attrs
  deriving newtype (Show, ToJSON, FromJSON)

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
ability attrs = base { abilityLimit = PerRound }
 where
  base = mkAbility (toSource attrs) 1 (ReactionAbility (WhenEnemyDefeated You))

instance (ActionRunner env investigator) => HasActions env investigator RolandBanks where
  getActions i (WhenEnemyDefeated You) (RolandBanks a@Attrs {..})
    | getId () i == investigatorId = do
      let ability' = (investigatorId, ability a)
      clueCount' <- asks $ unClueCount . getCount investigatorLocation
      unused <- asks $ notElem ability' . map unUsedAbility . getList ()
      pure
        [ uncurry ActivateCardAbilityAction ability'
        | unused && clueCount' > 0
        ]
  getActions _ _ _ = pure []

instance (InvestigatorRunner Attrs env) => RunMessage env RolandBanks where
  runMessage msg rb@(RolandBanks attrs@Attrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 | iid == investigatorId ->
      rb <$ unshiftMessage
        (DiscoverCluesAtLocation investigatorId investigatorLocation 1)
    ResolveToken ElderSign iid | iid == investigatorId -> do
      locationClueCount <- asks $ unClueCount . getCount investigatorLocation
      rb <$ runTest investigatorId (TokenValue ElderSign locationClueCount)
    _ -> RolandBanks <$> runMessage msg attrs
