module Arkham.Asset.Cards.JeremiahKirbyArcticArchaeologist (
  jeremiahKirbyArcticArchaeologist,
  JeremiahKirbyArcticArchaeologist (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message (SearchType (..))
import Arkham.Modifier
import Arkham.Strategy

newtype JeremiahKirbyArcticArchaeologist = JeremiahKirbyArcticArchaeologist AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahKirbyArcticArchaeologist :: AssetCard JeremiahKirbyArcticArchaeologist
jeremiahKirbyArcticArchaeologist = ally JeremiahKirbyArcticArchaeologist Cards.jeremiahKirbyArcticArchaeologist (2, 1)

instance HasModifiersFor JeremiahKirbyArcticArchaeologist where
  getModifiersFor (InvestigatorTarget iid) (JeremiahKirbyArcticArchaeologist a) | iid `controls` a = do
    pure $ toModifiers a [SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities JeremiahKirbyArcticArchaeologist where
  getAbilities (JeremiahKirbyArcticArchaeologist a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ AssetEntersPlay #when (be a)
    ]

instance RunMessage JeremiahKirbyArcticArchaeologist where
  runMessage msg a@(JeremiahKirbyArcticArchaeologist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- technically the choose is part of the cost, but I don't think we care
      let source = attrs.ability 1
      let revealTopOfDeck mtch =
            Search
              Revealing
              iid
              source
              (toTarget iid)
              [(FromTopOfDeck 5, ShuffleBackIn)]
              mtch
              (DrawAllFound iid)
      chooseOne
        iid
        [ Label "Even" [revealTopOfDeck CardWithEvenCost]
        , Label "Odd" [revealTopOfDeck CardWithOddCost]
        ]
      pure a
    _ -> JeremiahKirbyArcticArchaeologist <$> liftRunMessage msg attrs
