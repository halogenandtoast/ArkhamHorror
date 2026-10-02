module Arkham.Homebrew.DarkMatter.Assets.SpecialRelativity (specialRelativity) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose

newtype SpecialRelativity = SpecialRelativity AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

specialRelativity :: AssetCard SpecialRelativity
specialRelativity = asset SpecialRelativity Cards.specialRelativity

{- | "[action] Take 1 direct horror: Peek at the other side of any location. If
it is... a revealed location, flip that location. ...anything else, Move. Move
to that location."
-}
instance HasAbilities SpecialRelativity where
  getAbilities (SpecialRelativity a) =
    [controlled_ a 1 $ actionAbilityWithCost (DirectHorrorCost (a.ability 1) You 1)]

instance RunMessage SpecialRelativity where
  runMessage msg a@(SpecialRelativity attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select Anywhere
      chooseTargetM iid locations \lid -> do
        {- "If it is a revealed location, flip that location." The other side of
        an unrevealed location *is* its revealed side, so the usual case is a
        remote reveal; a location whose two faces are both locations (the
        Fragment of Carcosa caves) flips instead. A revealed single-sided
        location's other side is the scanning back, which is "anything else". -}
        unrevealed <- lid <=~> UnrevealedLocation
        flippable <- lid <=~> LocationCanBeFlipped
        if
          | unrevealed -> revealBy iid lid
          | flippable -> push $ Flip iid (attrs.ability 1) (toTarget lid)
          -- free: the ability's horror paid for the move
          | otherwise -> push $ Msg.MoveAction iid lid Free False
      pure a
    _ -> SpecialRelativity <$> liftRunMessage msg attrs
