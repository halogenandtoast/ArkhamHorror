module Arkham.Homebrew.DarkMatter.Treacheries.DarkReflections (
  darkReflectionsMalingerer,
  darkReflectionsMurderer,
  darkReflectionsSycophant,
  darkReflectionsZealot,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Discard
import Arkham.Helpers.Message.Discard (discardFromHand)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted hiding (DeckHasNoCards)

{- | The four Dark Reflections weaknesses share a shell:

"Hidden. Peril. / Revelation - Secretly add this card to your hand. / Forced -
After you reshuffle your deck because there are no cards in it: Discard this card
and take 3 horror (Malingerer, Murderer) or 3 damage (Sycophant, Zealot)."

and differ in the ability that lets you be rid of them.
-}
newtype DarkReflections = DarkReflections TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkDarkReflections :: CardDef -> TreacheryCard DarkReflections
mkDarkReflections = treachery DarkReflections

darkReflectionsMalingerer :: TreacheryCard DarkReflections
darkReflectionsMalingerer = mkDarkReflections Cards.darkReflectionsMalingerer

darkReflectionsMurderer :: TreacheryCard DarkReflections
darkReflectionsMurderer = mkDarkReflections Cards.darkReflectionsMurderer

darkReflectionsSycophant :: TreacheryCard DarkReflections
darkReflectionsSycophant = mkDarkReflections Cards.darkReflectionsSycophant

darkReflectionsZealot :: TreacheryCard DarkReflections
darkReflectionsZealot = mkDarkReflections Cards.darkReflectionsZealot

reflectionIs :: TreacheryAttrs -> CardDef -> Bool
reflectionIs a def = toCardCode (toCardDef a) == toCardCode def

instance HasAbilities DarkReflections where
  getAbilities (DarkReflections a) =
    [ mkAbility a 1 $ forced $ DeckHasNoCards #after You
    , restricted a 2 (criteriaFor a) (abilityTypeFor a)
    ]
   where
    criteriaFor attrs
      | reflectionIs attrs Cards.darkReflectionsMurderer =
          exists $ #ally <> DiscardableAsset <> AssetControlledBy (InvestigatorAt YourLocation)
      | reflectionIs attrs Cards.darkReflectionsSycophant =
          exists $ InvestigatorAt YourLocation <> InvestigatorWithResources (atLeast 4)
      | reflectionIs attrs Cards.darkReflectionsZealot = exists $ InvestigatorAt YourLocation
      | otherwise = NoRestriction
    abilityTypeFor attrs
      | reflectionIs attrs Cards.darkReflectionsMalingerer =
          freeReaction $ PhaseBegins #when #investigation
      | otherwise = actionAbility

instance RunMessage DarkReflections where
  runMessage msg t@(DarkReflections attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      if reflectionIs attrs Cards.darkReflectionsSycophant || reflectionIs attrs Cards.darkReflectionsZealot
        then assignDamage iid attrs 3
        else assignHorror iid attrs 3
      toDiscardBy iid attrs attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      if
        | reflectionIs attrs Cards.darkReflectionsMalingerer -> do
            investigators <- select UneliminatedInvestigator
            chooseTargetM iid investigators \victim -> do
              push $ LoseActions victim (attrs.ability 2) 2
              drawEncounterCard victim (attrs.ability 2)
        | reflectionIs attrs Cards.darkReflectionsMurderer -> do
            allies <- select $ #ally <> DiscardableAsset <> AssetControlledBy (InvestigatorAt YourLocation)
            chooseTargetM iid allies \ally -> push $ toMessage $ Discard (Just iid) (attrs.ability 2) (toTarget ally)
        | reflectionIs attrs Cards.darkReflectionsSycophant -> do
            payers <- select $ InvestigatorAt YourLocation <> InvestigatorWithResources (atLeast 4)
            chooseTargetM iid payers \payer -> spendResources payer 4
        | otherwise -> do
            victims <- select $ InvestigatorAt YourLocation
            chooseTargetM iid victims \victim ->
              push $ toMessage $ discardFromHand victim (attrs.ability 2) DiscardChoose 3
      toDiscardBy iid attrs attrs
      pure t
    _ -> DarkReflections <$> liftRunMessage msg attrs
