module Arkham.Homebrew.TheInvisibleMan.Treacheries.DoublingBack (doublingBack) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype DoublingBack = DoublingBack TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doublingBack :: TreacheryCard DoublingBack
doublingBack = treachery DoublingBack Cards.doublingBack

instance HasModifiersFor DoublingBack where
  getModifiersFor (DoublingBack a) = do
    modifySelect a Anyone [CannotDiscoverCluesAt $ not_ nearGriffin]
    modifySelect a (connectedTo griffinsLocation) [ShroudModifier 1]
   where
    griffinsLocation = LocationWithEnemy griffin
    nearGriffin = oneOf [griffinsLocation, connectedTo griffinsLocation]

instance HasAbilities DoublingBack where
  getAbilities (DoublingBack a) =
    [ scenarioI18n
        $ withI18nTooltip "doublingBack.discard"
        $ mkAbility a 1
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage DoublingBack where
  runMessage msg t@(DoublingBack attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> DoublingBack <$> liftRunMessage msg attrs
