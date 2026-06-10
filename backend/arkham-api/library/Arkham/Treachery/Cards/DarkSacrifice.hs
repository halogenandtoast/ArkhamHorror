module Arkham.Treachery.Cards.DarkSacrifice (darkSacrifice) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Matcher hiding (AssetDefeated, InvestigatorDefeated)
import Arkham.Matcher qualified as Matcher
import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype DarkSacrifice = DarkSacrifice TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkSacrifice :: TreacheryCard DarkSacrifice
darkSacrifice = treachery DarkSacrifice Cards.darkSacrifice

instance HasAbilities DarkSacrifice where
  getAbilities (DarkSacrifice a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ ScenarioCountIncremented #after StrengthOfTheAbyss
    , restricted a 2 (InThreatAreaOf You)
        $ forced
        $ oneOf
          [ Matcher.InvestigatorDefeated #after (BySource $ SourceIs $ toSource a) You
          , Matcher.AssetDefeated #after (BySource $ SourceIs $ toSource a) (#ally <> AssetControlledBy You)
          ]
    , restricted a 3 (InThreatAreaOf You)
        $ freeReaction
        $ ScenarioCountDecremented #after StrengthOfTheAbyss
    ]

instance RunMessage DarkSacrifice where
  runMessage msg t@(DarkSacrifice attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasCopy <- selectAny $ treacheryIs Cards.darkSacrifice <> TreacheryInThreatAreaOf (InvestigatorWithId iid)
      if hasCopy
        then do
          gainSurge attrs
          toDiscard attrs attrs
        else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure t
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.InvestigatorDefeated _ iid -> investigatorTakenByTheAbyss iid
        Window.AssetDefeated aid _ -> assetTakenByTheAbyss aid
        _ -> pure ()
      pure t
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      toDiscardBy iid (attrs.ability 3) attrs
      pure t
    _ -> DarkSacrifice <$> liftRunMessage msg attrs
