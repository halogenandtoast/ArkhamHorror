module Arkham.Homebrew.AgesUnwound.Treacheries.TimeLoop (timeLoop) where

import Arkham.Action qualified as Action
import Arkham.Calculation
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Target
import Arkham.Treachery.Import.Lifted

newtype TimeLoop = TimeLoop TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeLoop :: TreacheryCard TimeLoop
timeLoop = treachery TimeLoop Cards.timeLoop

-- | The seven action types the card offers, in printed order.
choices :: [(Text, Action.Action)]
choices =
  [ ("timeLoop.fight", Action.Fight)
  , ("timeLoop.move", Action.Move)
  , ("timeLoop.evade", Action.Evade)
  , ("timeLoop.investigate", Action.Investigate)
  , ("timeLoop.play", Action.Play)
  , ("timeLoop.draw", Action.Draw)
  , ("timeLoop.resource", Action.Resource)
  ]

{- | "Revelation - Test [willpower] (3). If you fail, choose one of: fight, move,
evade, investigate, play, draw, or resource. You can only perform actions of
the chosen type this round."

@MustTakeAction@ is the engine's "only this kind of action": @canDo@ reads it as
the negation of the matcher it carries, so every other action type is barred
(the same modifier Court of the Great Old Ones uses for its one-action-type
haunted).
-}
instance RunMessage TimeLoop where
  runMessage msg t@(TimeLoop attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ campaignI18n do
        for_ choices \(lbl, action) ->
          labeled lbl $ roundModifier attrs iid (MustTakeAction $ IsAction action)
      pure t
    _ -> TimeLoop <$> liftRunMessage msg attrs
