module Arkham.Homebrew.AgesUnwound.Acts.OnTheLam (onTheLam) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card (genCard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern ArkhamStreetsDeck)
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Matcher

newtype OnTheLam = OnTheLam ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLam :: ActCard OnTheLam
onTheLam = act (1, A) OnTheLam Cards.onTheLam Nothing

instance HasAbilities OnTheLam where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ triggered redirectableMove Free
    , campaignI18n
        $ withI18nTooltip "onTheLam.move"
        $ restricted
          (proxied YouSource a)
          2
          (notExists AccessibleLocation <> ScenarioDeckWithCard ArkhamStreetsDeck)
        $ ActionAbility #move Nothing (ActionCost 1)
    , restricted a 3 (exists EmptyLocation) $ forced $ RoundEnds #when
    ]

instance RunMessage OnTheLam where
  runMessage msg a@(OnTheLam attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      redirectToNewArkhamStreetsLocation (attrs.ability 1) iid
      pure a
    UseThisAbility iid (isProxySource attrs -> True) 2 -> do
      seedRedirectableMove (attrs.ability 2) iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      cards <- selectForM EmptyLocation \lid -> do
        removeLocationWithoutVictory lid
        fetchCard lid
      shuffleCardsIntoDeck ArkhamStreetsDeck cards
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push . ReplaceAct attrs.id =<< genCard Cards.onTheLam
      pure a
    _ -> OnTheLam <$> liftRunMessage msg attrs
