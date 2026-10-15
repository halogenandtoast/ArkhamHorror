module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.HiddenParlor (hiddenParlor) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.Homebrew.ThePhantomOfTheOpera.Traits
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype HiddenParlor = HiddenParlor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenParlor :: LocationCard HiddenParlor
hiddenParlor = symbolLabel $ location HiddenParlor Cards.hiddenParlor 3 (PerPlayer 2)

instance HasModifiersFor HiddenParlor where
  getModifiersFor (HiddenParlor a) =
    whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities HiddenParlor where
  getAbilities (HiddenParlor a) =
    if a.revealed
      then
        extendRevealed
          a
          [ scenarioI18n
              $ withI18nTooltip "hiddenParlor.peer"
              $ restricted a 1 (Here <> exists ConcealedCardAny)
              $ FastAbility
                (OrCost [DamageCost (a.ability 1) YouTarget 1, HorrorCost (a.ability 1) YouTarget 1])
          , scenarioI18n
              $ withI18nTooltip "hiddenParlor.dread"
              $ restricted
                a
                2
                ( Here
                    <> EnemyCount (AtLeast $ PerPlayer 1) (EnemyWithPlacement InTheShadows <> NonEliteEnemy)
                    <> TokensOnThis #horror (EqualTo $ Static 0)
                )
              $ FastAbility Free
          ]
      else
        extendUnrevealed1 a
          $ scenarioI18n
          $ withI18nTooltip "hiddenParlor.openTheWay"
          $ mkAbility a 3
          $ forced
          $ RevealLocation #after Anyone (locationIs Cards.organChamber)

instance RunMessage HiddenParlor where
  runMessage msg l@(HiddenParlor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      concealed <- select ConcealedCardAny
      chooseTargetM iid concealed $ revealConcealed iid (attrs.ability 1) . (.id)
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      placeTokens (attrs.ability 2) attrs #horror 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      reveal attrs
      doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 3) -> do
      redistributeConcealed (LocationWithTrait Underground)
      pure l
    _ -> HiddenParlor <$> liftRunMessage msg attrs
