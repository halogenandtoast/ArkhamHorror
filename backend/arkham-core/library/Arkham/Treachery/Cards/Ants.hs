module Arkham.Treachery.Cards.Ants (
  ants,
  Ants (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
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
      push $ revelationSkillTest iid attrs #agility 3
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ RevelationChoice iid (toSource attrs) n
      pure t
    RevelationChoice iid (isSource attrs -> True) n | n > 0 -> do
      hasDiscardableAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      push
        $ chooseOrRunOne iid
        $ Label "Discard hand card" [toMessage $ chooseAndDiscardCard iid attrs]
        : [ Label
            "Discard a card from your play area"
            [ChooseAndDiscardAsset iid (toSource attrs) DiscardableAsset]
          | hasDiscardableAssets
          ]
      pure t
    _ -> Ants <$> runMessage msg attrs
