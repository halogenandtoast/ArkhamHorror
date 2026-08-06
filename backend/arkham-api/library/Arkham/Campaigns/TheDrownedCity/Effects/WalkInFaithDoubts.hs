module Arkham.Campaigns.TheDrownedCity.Effects.WalkInFaithDoubts where

import Arkham.Ability
import Arkham.Classes.Entity (Entity)
import Arkham.Classes.HasAbilities (HasAbilities (..))
import Arkham.Classes.HasModifiersFor (HasModifiersFor)
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.RunMessage.Internal (RunMessage (..), liftRunMessage)
import Arkham.Draw.Types (newCardDraw)
import Arkham.Effect.Import
import Arkham.Effect.Types qualified as Effect
import Arkham.Helpers.Modifiers (ModifierType (..), hasModifier)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message (Message (..), pattern UseThisAbility)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Source (Sourceable (..))

{- | Walk in Faith, "I have such doubts": "The first time each investigator would
draw from the encounter deck in the next scenario, they may draw 1 card from their
deck instead."

One of these is created per investigator during The Apiary's intro — "the next
scenario" in a Task story read before setup is the scenario being set up.

The ability is a 'SilentForcedAbility' rather than a reaction even though the swap
is optional: the trigger is "the first time … would draw", so it is spent on that
draw whether or not the option is taken. Forcing it means the effect always gets to
present the choice and then disable itself; a declinable reaction would leave the
effect alive for a later draw.
-}
newtype WalkInFaithDoubtsEffect = WalkInFaithDoubtsEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walkInFaithDoubtsEffect :: EffectArgs -> WalkInFaithDoubtsEffect
walkInFaithDoubtsEffect (effectId, builder) =
  WalkInFaithDoubtsEffect $ Effect.baseAttrs "walkInFaithDoubts" effectId builder

instance HasAbilities WalkInFaithDoubtsEffect where
  getAbilities (WalkInFaithDoubtsEffect a) = case a.target.investigator of
    Nothing -> []
    Just iid -> [mkAbility a 1 $ SilentForcedAbility $ WouldDrawEncounterCard #when (be iid) #any]

instance RunMessage WalkInFaithDoubtsEffect where
  runMessage msg e@(WalkInFaithDoubtsEffect attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Only offer the swap to someone who can actually draw; otherwise it is a
      -- dead button and the encounter card comes anyway.
      cannotDraw <- hasModifier iid CannotDrawCards
      withI18n $ scope "theDrownedCity" $ chooseOneM iid do
        unless cannotDraw
          $ labeled' "walkInFaith.drawFromDeck"
          $ push
          $ ReplaceCurrentCardDraw iid
          $ newCardDraw (toSource attrs) iid 1
        labeled' "walkInFaith.drawEncounter" nothing
      disableReturn e
    _ -> WalkInFaithDoubtsEffect <$> liftRunMessage msg attrs
