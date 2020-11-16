{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.SkidsOToole
  ( SkidsOToole(..)
  , skidsOToole
  )
where

import Arkham.Import

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype SkidsOToole = SkidsOToole Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env SkidsOToole where
  getModifiersFor source target (SkidsOToole attrs) =
    getModifiersFor source target attrs

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

instance ActionRunner env => HasActions env SkidsOToole where
  getActions iid (DuringTurn You) (SkidsOToole a@Attrs {..})
    | iid == investigatorId = do
      let ability' = (investigatorId, ability a)
      unused <- asks $ notElem ability' . map unUsedAbility . getList ()
      pure
        [ uncurry ActivateCardAbilityAction ability'
        | unused && investigatorResources >= 2
        ]
  getActions _ _ _ = pure []

instance HasTokenValue env SkidsOToole where
  getTokenValue (SkidsOToole attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue (SkidsOToole attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env SkidsOToole where
  runMessage msg i@(SkidsOToole attrs@Attrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 | iid == investigatorId ->
      pure . SkidsOToole $ attrs & resources -~ 2 & remainingActions +~ 1
    PassedSkillTest iid _ _ (DrawnTokenTarget token) _
      | iid == investigatorId && drawnTokenFace token == ElderSign -> i
      <$ unshiftMessage (TakeResources iid 2 False)
    _ -> SkidsOToole <$> runMessage msg attrs
