module Arkham.Homebrew.AgesUnwound.Treacheries.CurseOfAThousandWinters (
  curseOfAThousandWinters,
) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Token qualified as Token
import Arkham.Treachery.Import.Lifted
import Arkham.Treachery.Types (treacheryResources)

newtype CurseOfAThousandWinters = CurseOfAThousandWinters TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Curse of a Thousand Winters/ (@:ages-unwound:123@). The weakness Scenario
V's Resolution 1 hands out when /the Myriad weaved a dread curse/: a countdown
in resources that costs an action per turn until it runs out.
-}
curseOfAThousandWinters :: TreacheryCard CurseOfAThousandWinters
curseOfAThousandWinters = treachery CurseOfAThousandWinters Cards.curseOfAThousandWinters

{- | "__Forced__ - At the start of your turn: Lose 1 action. Remove 1 resource
from Curse of a Thousand Winters. Then, if there are no resources on Curse of a
Thousand Winters, discard it."
-}
instance HasAbilities CurseOfAThousandWinters where
  getAbilities (CurseOfAThousandWinters a) =
    [restricted a 1 InYourThreatArea $ forced $ TurnBegins #when You]

instance RunMessage CurseOfAThousandWinters where
  runMessage msg t@(CurseOfAThousandWinters attrs) = runQueueT $ case msg of
    {- "__Revelation__ - Test [willpower] (4). If you fail, put Curse of a
    Thousand Winters into play in your threat area with X resources on, where X
    is the amount you failed by." -}
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      placeInThreatArea attrs iid
      placeTokens attrs attrs Token.Resource n
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseStandardActions iid (attrs.ability 1) 1
      removeTokens (attrs.ability 1) attrs Token.Resource 1
      {- The removal is still queued, so the card's own count is read here: one
      resource is coming off, which leaves none exactly when it had at most one. -}
      when (treacheryResources attrs <= 1) $ toDiscard (attrs.ability 1) attrs
      pure t
    _ -> CurseOfAThousandWinters <$> liftRunMessage msg attrs
