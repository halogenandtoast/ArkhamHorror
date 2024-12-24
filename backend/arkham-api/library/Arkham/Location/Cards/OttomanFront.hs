module Arkham.Location.Cards.OttomanFront (ottomanFront) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Keyword (Keyword (Retaliate))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Eidolon))

newtype OttomanFront = OttomanFront LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ottomanFront :: LocationCard OttomanFront
ottomanFront = location OttomanFront Cards.ottomanFront 4 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.memoryOfARavagedCountry]

instance HasAbilities OttomanFront where
  getAbilities (OttomanFront a) =
    extendRevealed a [mirage a 2 mirageCards]

instance HasModifiersFor OttomanFront where
  getModifiersFor (OttomanFront a) = do
    modifySelfWhenM
      a
      ( selectAny
          $ mapOneOf
            assetIs
            [Assets.jamesCookieFredericksDubiousChoice, Assets.jamesCookieFredericksDubiousChoiceResolute]
          <> at_ (be a)
      )
      [ShroudModifier (-2)]
    modifySelect a (EnemyWithTrait Eidolon <> at_ (be a)) [EnemyFight 2, RemoveKeyword Retaliate]
    clearedOfMirages a mirageCards

instance RunMessage OttomanFront where
  runMessage msg (OttomanFront attrs) = runQueueT $ case msg of
    _ -> OttomanFront <$> mirageRunner Stories.ottomanFront mirageCards 2 msg attrs
