module Arkham.Location.Cards.InfirmaryFatalMirage (infirmaryFatalMirage) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype InfirmaryFatalMirage = InfirmaryFatalMirage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmaryFatalMirage :: LocationCard InfirmaryFatalMirage
infirmaryFatalMirage = location InfirmaryFatalMirage Cards.infirmaryFatalMirage 4 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.memoryOfALostPatient]

instance HasModifiersFor InfirmaryFatalMirage where
  getModifiersFor (InfirmaryFatalMirage a) = do
    modifySelfWhenM
      a
      ( selectAny
          $ mapOneOf
            assetIs
            [ Assets.drMalaSinhaDaringPhysician
            , Assets.drMalaSinhaDaringPhysicianResolute
            ]
          <> at_ (be a)
      )
      [ShroudModifier (-2)]
    clearedOfMirages a mirageCards

instance HasAbilities InfirmaryFatalMirage where
  getAbilities (InfirmaryFatalMirage a) =
    extendRevealed
      a
      [mirage a 2 mirageCards, playerLimit PerGame $ mkAbility a 1 $ forced $ Enters #after You (be a)]

instance RunMessage InfirmaryFatalMirage where
  runMessage msg l@(InfirmaryFatalMirage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- field InvestigatorRemainingHealth iid
      if n > 6
        then doStep 1 msg
        else assignDamage iid attrs 1
      pure l
    DoStep _ msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      n <- field InvestigatorRemainingHealth iid
      when (n > 6) do
        assignDamage iid attrs 1
        doStep 1 msg'
      pure l
    _ -> InfirmaryFatalMirage <$> mirageRunner Stories.infirmary mirageCards 2 msg attrs
