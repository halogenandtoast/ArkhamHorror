module Arkham.Asset.Cards.AlyssaGraham
  ( alyssaGraham
  , AlyssaGraham(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype AlyssaGraham = AlyssaGraham AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alyssaGraham :: AssetCard AlyssaGraham
alyssaGraham = ally AlyssaGraham Cards.alyssaGraham (1, 3)

instance HasAbilities AlyssaGraham where
  getAbilities (AlyssaGraham a) =
    [ restrictedAbility a 1 OwnsThis $ FastAbility $ Costs
        [ExhaustCost (toTarget a)]
    ]

instance HasModifiersFor env AlyssaGraham where
  getModifiersFor _ (InvestigatorTarget iid) (AlyssaGraham a) =
    pure [ toModifier a (SkillModifier SkillIntellect 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env AlyssaGraham where
  runMessage msg a@(AlyssaGraham attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      targets <- map InvestigatorTarget <$> getSetList ()
      a <$ push
        (chooseOne iid
        $ Search
            iid
            source
            EncounterDeckTarget
            [fromTopOfDeck 1]
            AnyCard
            (DeferSearchedToTarget $ toTarget attrs)
        : [ Search
              iid
              source
              target
              [fromTopOfDeck 1]
              AnyCard
              (DeferSearchedToTarget $ toTarget attrs)
          | target <- targets
          ]
        )
    SearchFound iid target deck cards | isTarget attrs target -> a <$ pushAll
      [ FocusCards cards
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
