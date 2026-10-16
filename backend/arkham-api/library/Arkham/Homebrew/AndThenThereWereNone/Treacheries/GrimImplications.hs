{- | "__Revelation__ - If it is act 2 or there are no Victim assets without cards
attached, Grim Implications gains surge. Otherwise, attach Grim Implications to
the furthest Victim asset without a Method card attached."

'farthestVictimsWithoutMethod' answers both halves at once: it is empty exactly
when no victim is still missing a method.
-}
module Arkham.Homebrew.AndThenThereWereNone.Treacheries.GrimImplications (
  grimImplications,
) where

import Arkham.Ability
import Arkham.Helpers.Act (currentActStepIs)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype GrimImplications = GrimImplications TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grimImplications :: TreacheryCard GrimImplications
grimImplications = treachery GrimImplications Cards.grimImplications

{- | "__Forced__ - At the end of the round, if there is 1 or more doom on this
card, place 1 doom on the current agenda. Otherwise, place 1 doom on this card."

"{reaction} After a Method card is revealed at the attached asset's location:
Discard Grim Implications." A revealed method is turned face up and put into
play as an asset, so that is the window; the reaction only exists while attached,
which is what "the attached asset's location" requires.
-}
instance HasAbilities GrimImplications where
  getAbilities (GrimImplications a) =
    scenarioI18n
      $ withI18nTooltip "grimImplications.doom" (mkAbility a 1 $ forced $ RoundEnds #when)
      : [ withI18nTooltip "grimImplications.solved"
            $ mkAbility a 2
            $ freeReaction
            $ AssetEntersPlay #after (methodAsset <> AssetAt (locationWithAsset aid))
        | aid <- toList a.attached.asset
        ]

instance RunMessage GrimImplications where
  runMessage msg t@(GrimImplications attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      isAct2 <- currentActStepIs 2
      candidates <- farthestVictimsWithoutMethod iid
      if isAct2 || null candidates
        then gainSurge attrs
        else chooseTargetM iid candidates (attachTreachery attrs)
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      if attrs.doom >= 1
        then placeDoomOnAgendaBy (attrs.ability 1) 1
        else placeDoom (attrs.ability 1) attrs 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> GrimImplications <$> liftRunMessage msg attrs
