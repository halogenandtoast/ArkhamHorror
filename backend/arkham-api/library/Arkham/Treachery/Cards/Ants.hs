module Arkham.Treachery.Cards.Ants (ants, Ants (..)) where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Ants = Ants TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ants :: TreacheryCard Ants
ants = treachery Ants Cards.ants

-- Interpretation, since this card avoids the word must for hand discard and
-- targetting language for the in play we can choose to discard a hand card
-- even if our hand is empty

instance RunMessage Ants where
  runMessage msg t@(Ants attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      push $ DoStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTestBy iid (isSource attrs -> True) _) | n > 0 -> do
      hasDiscardableAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label "Discard hand card" [toMessage $ randomDiscard iid attrs, DoStep (n - 1) msg']
        : [ Label
            "Discard a card from your play area"
            [ChooseAndDiscardAsset iid (toSource attrs) DiscardableAsset, DoStep (n - 1) msg']
          | hasDiscardableAssets
          ]
      pure t
    _ -> Ants <$> runMessage msg attrs
