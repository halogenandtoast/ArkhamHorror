{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.SkidsOToole
  ( SkidsOToole(..)
  , skidsOToole
  )
where

import Arkham.Import

import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait

newtype SkidsOToole = SkidsOToole Attrs
  deriving newtype (Show, ToJSON, FromJSON)

skidsOToole :: SkidsOToole
skidsOToole = SkidsOToole $ baseAttrs
  "01003"
  "\"Skids\" O'Toole"
  Rogue
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 3
    , combat = 3
    , agility = 4
    }
  [Criminal]

ability :: Attrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (FastAbility (DuringTurn You))

instance (ActionRunner env investigator) => HasActions env investigator SkidsOToole where
  getActions i (DuringTurn You) (SkidsOToole a@Attrs {..})
    | getId () i == investigatorId = do
      let ability' = (getId () i, ability a)
      unused <- asks $ notElem ability' . map unUsedAbility . getList ()
      pure
        [ uncurry ActivateCardAbilityAction ability'
        | unused && resourceCount i >= 2
        ]
  getActions _ _ _ = pure []

instance (InvestigatorRunner Attrs env) => RunMessage env SkidsOToole where
  runMessage msg i@(SkidsOToole attrs@Attrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 | iid == investigatorId -> do
      pure . SkidsOToole $ attrs & resources -~ 2 & remainingActions +~ 1
    ResolveToken ElderSign iid | iid == investigatorId -> do
      i <$ runTest investigatorId (TokenValue ElderSign 2)
    PassedSkillTest iid _ _ (TokenTarget ElderSign) _ | iid == investigatorId ->
      do
        i <$ unshiftMessage (TakeResources iid 2 False)
    _ -> SkidsOToole <$> runMessage msg attrs
