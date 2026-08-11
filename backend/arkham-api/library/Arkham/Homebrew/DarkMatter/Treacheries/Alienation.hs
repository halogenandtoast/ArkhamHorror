module Arkham.Homebrew.DarkMatter.Treacheries.Alienation (alienation) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Location.Types (Field (LocationShroud))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype Alienation = Alienation TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienation :: TreacheryCard Alienation
alienation = treachery Alienation Cards.alienation

-- | "Your maximum hand size is reduced by the shroud value of your location."
instance HasModifiersFor Alienation where
  getModifiersFor (Alienation a) = for_ a.inThreatAreaOf \iid -> do
    shroud <-
      selectOne (locationWithInvestigator iid) >>= maybe (pure 0) (fieldWithDefault 0 LocationShroud)
    inThreatAreaGets a [HandSize (negate shroud)]

{- | "[action]: Investigate. If you succeed, instead of discovering clues, discard
Alienation."
-}
instance HasAbilities Alienation where
  getAbilities (Alienation a) = [investigateAbility a 1 mempty Here]

instance RunMessage Alienation where
  runMessage msg t@(Alienation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      -- "instead of discovering clues": redirect the successful investigation
      -- to this treachery as a proxy target
      withLocationOf iid \lid ->
        skillTestModifier
          sid
          (attrs.ability 1)
          lid
          (AlternateSuccessfullInvestigation $ ProxyTarget (toTarget attrs) (toTarget attrs))
      investigateEdit_ sid iid (attrs.ability 1) (setTarget attrs)
      pure t
    Successful (Action.Investigate, _) iid _ (ProxyTarget (isTarget attrs -> True) _) _ -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Alienation <$> liftRunMessage msg attrs
