module Arkham.Homebrew.AgesUnwound.Acts.WhatCameBefore (whatCameBefore) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (ShroudModifier), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (isAtOrPastFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Traits (pattern Interior)
import Arkham.Matcher

newtype WhatCameBefore = WhatCameBefore ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 1c --- the first act of the __past-selves deck, which is act deck 2__.

Deck 2 is load-bearing: @night_of_the_ritual@'s /Backfire/ advances
@selectOne (ActWithDeckId 1)@ by hand, so making the past deck deck 1 would make
/Backfire/ advance the wrong deck. The side is C\/D rather than A\/B so the guide's
"act 1c" reads literally in the client.
-}
whatCameBefore :: ActCard WhatCameBefore
whatCameBefore = act (1, C) WhatCameBefore Cards.whatCameBefore Nothing

-- | "Each [[Interior]] location gets +2 shroud."
instance HasModifiersFor WhatCameBefore where
  getModifiersFor (WhatCameBefore a) =
    modifySelect a (LocationWithTrait Interior) [ShroudModifier 2]

{- | "Objective - When /the boundary is broken,/ advance."

The past deck advances on a campaign-log entry recorded __with a time__, compared
to the current @(agenda number, doom on agenda)@ pair after the doom threshold is
checked each round (guide p15). That comparison is a game query, not something
'Arkham.Criteria.Criterion' can express, so the condition is polled on the one
window the engine offers around the threshold check and the test lives in the
handler. 'SilentForcedAbility' because the hook is engine-only --- the printed
Objective is on the card face, and a plain @forced@ would announce a trigger every
mythos phase that the condition is not yet met.
-}
instance HasAbilities WhatCameBefore where
  getAbilities (WhatCameBefore a) =
    [mkAbility a 1 $ SilentForcedAbility $ MythosStep AfterCheckDoomThreshold]

instance RunMessage WhatCameBefore where
  runMessage msg a@(WhatCameBefore attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      whenM (isAtOrPastFor TheBoundaryIsBroken) $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      {- "Predestined Progress: Check your Campaign Log. If /the investigators used
      the school's front door,/ advance to Act 2c - Hellish Hound. Otherwise,
      advance to Act 2c - Warded Way."

      Both stage-2 acts sit in deck 2 at once; 'AdvanceToAct' drops the sibling
      because it filters the remaining stack to acts of a different stage. -}
      frontDoor <- getHasRecord TheInvestigatorsUsedTheSchoolsFrontDoor
      advanceToAct attrs (if frontDoor then Cards.hellishHound else Cards.wardedWay) C
      pure a
    _ -> WhatCameBefore <$> liftRunMessage msg attrs
