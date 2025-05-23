module Arkham.Asset.Assets.JimsTrumpet (jimsTrumpet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype JimsTrumpet = JimsTrumpet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jimsTrumpet :: AssetCard JimsTrumpet
jimsTrumpet = asset JimsTrumpet Cards.jimsTrumpet

instance HasAbilities JimsTrumpet where
  getAbilities (JimsTrumpet x) =
    [ controlled
        x
        1
        ( exists (HealableInvestigator (toSource x) #horror $ at_ (oneOf [YourLocation, ConnectedLocation]))
            <> DuringSkillTest AnySkillTest
        )
        $ triggered (RevealChaosToken #when Anyone #skull) (exhaust x)
    ]

instance RunMessage JimsTrumpet where
  runMessage msg a@(JimsTrumpet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.controller \controller -> do
        withLocationOf controller \loc -> do
          investigators <-
            select
              $ HealableInvestigator (attrs.ability 1) #horror
              $ oneOf [colocatedWith controller, InvestigatorAt (accessibleFrom loc)]

          chooseTargetM iid investigators \x -> healHorror x (attrs.ability 1) 1
      pure a
    _ -> JimsTrumpet <$> liftRunMessage msg attrs
