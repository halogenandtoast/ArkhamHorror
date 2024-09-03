module Arkham.Treachery.Cards.GlimpseOfTheUnderworld (glimpseOfTheUnderworld, GlimpseOfTheUnderworld (..)) where

import Arkham.Ability
import Arkham.Damage
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype GlimpseOfTheUnderworld = GlimpseOfTheUnderworld TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimpseOfTheUnderworld :: TreacheryCard GlimpseOfTheUnderworld
glimpseOfTheUnderworld = treachery GlimpseOfTheUnderworld Cards.glimpseOfTheUnderworld

instance HasAbilities GlimpseOfTheUnderworld where
  getAbilities (GlimpseOfTheUnderworld attrs) =
    [ restrictedAbility attrs 1 (InThreatAreaOf You)
        $ forced
        $ oneOf
          [ InvestigatorWouldTakeDamage #when You AnySource AnyDamageType
          , InvestigatorWouldTakeHorror #when You AnySource
          ]
    , fastAbility attrs 2 Free (InThreatAreaOf You)
    ]

toDamageType :: [Window] -> DamageType
toDamageType [] = error "invalid window"
toDamageType ((windowType -> wType) : rest) = case wType of
  Window.WouldTakeDamage {} -> DamageType
  Window.WouldTakeHorror {} -> HorrorType
  _ -> toDamageType rest

instance RunMessage GlimpseOfTheUnderworld where
  runMessage msg t@(GlimpseOfTheUnderworld attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (toDamageType -> damageType) _ -> do
      case damageType of
        DamageType -> dealAdditionalDamage iid 1 []
        HorrorType -> dealAdditionalHorror iid 1 []
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      assignDamageAndHorror iid (attrs.ability 2) 1 1
      pure t
    _ -> GlimpseOfTheUnderworld <$> liftRunMessage msg attrs
