module Arkham.Homebrew.AgesUnwound.Treacheries.AerialBombardment (aerialBombardment) where

import Arkham.Ability
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.Investigator (getCanLoseActions)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers (unstuckI18n)
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype AerialBombardment = AerialBombardment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aerialBombardment :: TreacheryCard AerialBombardment
aerialBombardment = treachery AerialBombardment Cards.aerialBombardment

{- | "Forced - When you leave attached location: Spend 1 action or take 1
damage." / "Forced - At the end of your turn, if you are at attached location:
For each doom in play (min 1), take 1 damage or discard 1 card from your hand at
random."
-}
instance HasAbilities AerialBombardment where
  getAbilities (AerialBombardment a) =
    [ mkAbility a 1 $ forced $ Leaves #when You (locationWithTreachery a)
    , mkAbility a 2 $ forced $ TurnEnds #when (You <> at_ (locationWithTreachery a))
    ]

instance RunMessage AerialBombardment where
  runMessage msg t@(AerialBombardment attrs) = runQueueT $ case msg of
    {- "Revelation - Attach to your location. Limit 1 per location."

    With a copy already attached here there is nowhere to attach to, so the card
    simply has no effect: the treachery is still in Limbo, which is what the
    engine's own @After (Revelation …)@ discards. -}
    Revelation iid (isSource attrs -> True) -> do
      selectOne
        ( locationWithInvestigator iid
            <> not_ (LocationWithTreachery $ treacheryIs Cards.aerialBombardment)
        )
        >>= traverse_ (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canLose <- getCanLoseActions iid
      chooseOrRunOneM iid $ unstuckI18n $ scope "aerialBombardment" do
        labeledValidate canLose "spendAnAction" $ loseStandardActions iid (attrs.ability 1) 1
        labeled "takeOneDamage" $ assignDamage iid (attrs.ability 1) 1
      pure t
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      doom <- getDoomCount
      doStep (max 1 doom) msg
      pure t
    {- Each point is its own choice, so this is the two-part `doStep` countdown
    and not a flat count: without `doNextStep` it would resolve exactly once. -}
    DoStep n (UseThisAbility iid (isSource attrs -> True) 2) | n > 0 -> do
      hasCards <- notNull <$> iid.hand
      chooseOrRunOneM iid $ unstuckI18n $ scope "aerialBombardment" do
        labeled "takeOneDamage" $ assignDamage iid (attrs.ability 2) 1
        labeledValidate hasCards "discardRandomCard" $ randomDiscard iid (attrs.ability 2)
      -- `msg`, not the inner UseThisAbility: doNextStep matches `DoStep n inner`,
      -- so handing it the unwrapped message silently ends the loop after one pass.
      doNextStep msg
      pure t
    _ -> AerialBombardment <$> liftRunMessage msg attrs
