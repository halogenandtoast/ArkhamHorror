module Arkham.Homebrew.TheInvisibleMan.Treacheries.ObviousSigns (obviousSigns) where

import Arkham.Ability
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.History.Types
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Location.Types (Field (LocationPrintedShroud))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Import.Lifted hiding (DiscoverClues)

newtype ObviousSigns = ObviousSigns TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obviousSigns :: TreacheryCard ObviousSigns
obviousSigns = treachery ObviousSigns Cards.obviousSigns

instance HasModifiersFor ObviousSigns where
  getModifiersFor (ObviousSigns a) = for_ a.attached.location \lid -> do
    shroud <- field LocationPrintedShroud lid >>= maybe (pure 0) getGameValue
    modifiedWhen_ a (shroud > 1) lid [ShroudModifier (-1)]

instance HasAbilities ObviousSigns where
  getAbilities (ObviousSigns a) = case a.attached.location of
    Nothing -> []
    Just lid ->
      [ scenarioI18n
          $ withI18nTooltip "obviousSigns.tracked"
          $ restricted a 1 (HasHistory RoundHistory You (CluesDiscoveredAt (atLeast 2) lid))
          $ forced
          $ DiscoverClues #after You (be lid) (atLeast 1)
      ]

instance RunMessage ObviousSigns where
  runMessage msg t@(ObviousSigns attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectWhenNotNull
        (LocationWithMostClues $ locationWithoutTreachery Cards.obviousSigns)
        \locations -> chooseOneM iid $ targets locations $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamageAndHorror iid (attrs.ability 1) 1 1
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> ObviousSigns <$> liftRunMessage msg attrs
