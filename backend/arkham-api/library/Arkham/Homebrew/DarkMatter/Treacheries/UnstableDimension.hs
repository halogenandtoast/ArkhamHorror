module Arkham.Homebrew.DarkMatter.Treacheries.UnstableDimension (unstableDimension) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype UnstableDimension = UnstableDimension TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unstableDimension :: TreacheryCard UnstableDimension
unstableDimension = treachery UnstableDimension Cards.unstableDimension

{- | "Revelation - Put this card into play in your threat area. / Forced - At the
end of your turn: Take 2 damage and flip your current location to its other side.
Then, discard this card. / [action] Spend 2 resources: Discard this card."
-}
instance HasAbilities UnstableDimension where
  getAbilities (UnstableDimension a) =
    [ mkAbility a 1 $ forced $ TurnEnds #when You
    , restricted a 2 (youExist $ InvestigatorWithResources $ atLeast 2)
        $ actionAbilityWithCost (ResourceCost 2)
    ]

instance RunMessage UnstableDimension where
  runMessage msg t@(UnstableDimension attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 2
      here <- select $ locationWithInvestigator iid
      for_ here \lid -> push $ Flip iid (toSource $ attrs.ability 1) (toTarget lid)
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> UnstableDimension <$> liftRunMessage msg attrs
