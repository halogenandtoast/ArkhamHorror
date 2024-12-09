module Arkham.Asset.Assets.JeremiahKirbyArcticArchaeologist (
  jeremiahKirbyArcticArchaeologist,
  JeremiahKirbyArcticArchaeologist (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher
import Arkham.Message (SearchType (..))
import Arkham.Strategy
import Arkham.Taboo

newtype JeremiahKirbyArcticArchaeologist = JeremiahKirbyArcticArchaeologist AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahKirbyArcticArchaeologist :: AssetCard JeremiahKirbyArcticArchaeologist
jeremiahKirbyArcticArchaeologist = ally JeremiahKirbyArcticArchaeologist Cards.jeremiahKirbyArcticArchaeologist (2, 1)

instance HasModifiersFor JeremiahKirbyArcticArchaeologist where
  getModifiersFor (JeremiahKirbyArcticArchaeologist a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities JeremiahKirbyArcticArchaeologist where
  getAbilities (JeremiahKirbyArcticArchaeologist a) =
    [ ( if tabooed TabooList21 a
          then limitedAbility (MaxPer Cards.jeremiahKirbyArcticArchaeologist PerGame 2)
          else id
      )
        $ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ AssetEntersPlay #when (be a)
    ]

instance RunMessage JeremiahKirbyArcticArchaeologist where
  runMessage msg a@(JeremiahKirbyArcticArchaeologist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- technically the choose is part of the cost, but I don't think we care
      let source = attrs.ability 1
      let revealTopOfDeck mtch =
            Search Revealing iid source (toTarget iid) [fromTopOfDeck 5] mtch (DrawAllFound iid)
      chooseOne
        iid
        [ Label "Even" [revealTopOfDeck $ basic CardWithEvenCost]
        , Label "Odd" [revealTopOfDeck $ basic CardWithOddCost]
        ]
      pure a
    _ -> JeremiahKirbyArcticArchaeologist <$> liftRunMessage msg attrs
