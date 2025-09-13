module Arkham.Location.Cards.WitchHauntedWoodsTheLonelyTree (witchHauntedWoodsTheLonelyTree) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Discard
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype WitchHauntedWoodsTheLonelyTree = WitchHauntedWoodsTheLonelyTree LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsTheLonelyTree :: LocationCard WitchHauntedWoodsTheLonelyTree
witchHauntedWoodsTheLonelyTree =
  location WitchHauntedWoodsTheLonelyTree Cards.witchHauntedWoodsTheLonelyTree 2 (PerPlayer 1)

instance HasModifiersFor WitchHauntedWoodsTheLonelyTree where
  getModifiersFor (WitchHauntedWoodsTheLonelyTree a) = modifySelectMaybe a Anyone \iid -> do
    handLength <- lift $ fieldMap InvestigatorHand length iid
    guard $ handLength >= 3 && handLength <= 5
    pure [CannotInvestigateLocation (toId a)]

instance HasAbilities WitchHauntedWoodsTheLonelyTree where
  getAbilities (WitchHauntedWoodsTheLonelyTree a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted
        a
        1
        ( Here
            <> exists
              ( oneOf [You, at_ (NotLocation YourLocation <> "Witch-Haunted Woods")]
                  <> InvestigatorWithDiscardableCard
              )
        )
      $ FastAbility Free

instance RunMessage WitchHauntedWoodsTheLonelyTree where
  runMessage msg l@(WitchHauntedWoodsTheLonelyTree attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let otherMatcher = InvestigatorAt ("Witch-Haunted Woods" <> not_ (locationWithInvestigator iid))

      forDraw <- select @InvestigatorMatcher $ can.draw.cards FromOtherSource otherMatcher
      forDiscard <- select $ InvestigatorWithDiscardableCard <> otherMatcher
      handLength <- fieldMap InvestigatorHand length iid
      canDraw <- can.draw.cards FromOtherSource iid

      chooseOrRunOneM iid do
        when (handLength > 0) do
          labeled
            "You choose and discard 1 card from your hand, then an investigator at a different Witch-Haunted Woods draws 1 card"
            do
              inner <- capture $ chooseTargetM iid forDraw \other -> drawCards other attrs 1
              chooseAndDiscardCardEdit iid attrs \d -> d {discardThen = guard (notNull inner) $> Run inner}

        when (notNull forDiscard && canDraw) do
          labeled "vice versa" do
            chooseOrRunOneM iid do
              inner <- capture $ drawCards iid attrs 1
              targets forDiscard \other ->
                chooseAndDiscardCardEdit other attrs \d -> d {discardThen = Just $ Run inner}
      pure l
    _ -> WitchHauntedWoodsTheLonelyTree <$> liftRunMessage msg attrs
