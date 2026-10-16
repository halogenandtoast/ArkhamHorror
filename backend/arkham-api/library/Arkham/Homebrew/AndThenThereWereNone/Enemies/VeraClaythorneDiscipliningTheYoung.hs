{- | The Governess, unmasked. She grows with the offerings and adds one to every
sigil each night, so she sharpens herself.
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.VeraClaythorneDiscipliningTheYoung (
  veraClaythorneDiscipliningTheYoung,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher

newtype VeraClaythorneDiscipliningTheYoung = VeraClaythorneDiscipliningTheYoung EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

veraClaythorneDiscipliningTheYoung :: EnemyCard VeraClaythorneDiscipliningTheYoung
veraClaythorneDiscipliningTheYoung =
  enemy VeraClaythorneDiscipliningTheYoung Cards.veraClaythorneDiscipliningTheYoung

instance HasModifiersFor VeraClaythorneDiscipliningTheYoung where
  -- "Vera Claythorne gets +1 fight and +1 evade for every 1{perPlayer} total
  -- offerings among copies of Antediluvian Sigils."
  getModifiersFor (VeraClaythorneDiscipliningTheYoung a) = do
    total <- totalOfferings
    per <- perPlayer 1
    let n = if per > 0 then total `div` per else 0
    modifySelfWhen a (n > 0) [EnemyFight n, EnemyEvade n]

instance HasAbilities VeraClaythorneDiscipliningTheYoung where
  getAbilities (VeraClaythorneDiscipliningTheYoung a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "veraClaythorneDiscipliningTheYoung.offerings"
      $ mkAbility a 1
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage VeraClaythorneDiscipliningTheYoung where
  runMessage msg e@(VeraClaythorneDiscipliningTheYoung attrs) = runQueueT $ case msg of
    -- "Add 1 offering to each copy of Antediluvian Sigils."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach antediluvianSigils $ placeTokensOn (attrs.ability 1) #offering 1
      pure e
    _ -> VeraClaythorneDiscipliningTheYoung <$> liftRunMessage msg attrs
