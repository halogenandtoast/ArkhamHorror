module Arkham.Types.Asset.Cards.AlyssaGraham
  ( alyssaGraham
  , AlyssaGraham(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype AlyssaGraham = AlyssaGraham AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alyssaGraham :: AssetCard AlyssaGraham
alyssaGraham = ally AlyssaGraham Cards.alyssaGraham (1, 3)

instance HasActions AlyssaGraham where
  getActions (AlyssaGraham a) =
    [restrictedAbility (toSource a) 1 OwnsThis (FastAbility ExhaustThis)]

instance HasModifiersFor env AlyssaGraham where
  getModifiersFor _ (InvestigatorTarget iid) (AlyssaGraham a) =
    pure [ toModifier a (SkillModifier SkillIntellect 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env (), HasSet InvestigatorId env ()) => RunMessage env AlyssaGraham where
  runMessage msg a@(AlyssaGraham attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      targets <- map InvestigatorTarget <$> getSetList ()
      a <$ push
        (chooseOne iid
        $ SearchTopOfDeck
            iid
            source
            EncounterDeckTarget
            1
            []
            (DeferSearchedToTarget $ toTarget attrs)
        : [ SearchTopOfDeck
              iid
              source
              target
              1
              []
              (DeferSearchedToTarget $ toTarget attrs)
          | target <- targets
          ]
        )
    SearchTopOfDeckFound iid target deck card | isTarget attrs target ->
      a <$ pushAll
        [ FocusCards [card]
        , chooseOne
          iid
          [ Label
            "Add 1 Doom to Alyssa to move card to bottom"
            [ UnfocusCards
            , PlaceDoom (toTarget attrs) 1
            , MoveTopOfDeckToBottom (toSource attrs) deck 1
            ]
          , Label "Leave card on top" [UnfocusCards]
          ]
        ]
    _ -> AlyssaGraham <$> runMessage msg attrs
