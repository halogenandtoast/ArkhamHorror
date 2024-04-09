module Arkham.Asset.Cards.JimsTrumpet (JimsTrumpet (..), jimsTrumpet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Prelude

newtype JimsTrumpet = JimsTrumpet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jimsTrumpet :: AssetCard JimsTrumpet
jimsTrumpet = asset JimsTrumpet Cards.jimsTrumpet

instance HasAbilities JimsTrumpet where
  getAbilities (JimsTrumpet x) =
    [ controlledAbility
        x
        1
        ( exists $ HealableInvestigator (toSource x) #horror $ at_ (oneOf [YourLocation, ConnectedLocation])
        )
        $ ReactionAbility (RevealChaosToken #when Anyone #skull) (exhaust x)
    ]

instance RunMessage JimsTrumpet where
  runMessage msg a@(JimsTrumpet attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      case attrs.controller of
        Just controller -> do
          location <- getJustLocation controller
          investigators <-
            select
              $ HealableInvestigator (attrs.ability 1) #horror
              $ oneOf [colocatedWith controller, InvestigatorAt (accessibleFrom location)]

          player <- getPlayer controller
          push
            $ chooseOne
              player
              [targetLabel iid [HealHorror (toTarget iid) (attrs.ability 1) 1] | iid <- investigators]
          pure a
        _ -> error "Invalid call"
    _ -> JimsTrumpet <$> runMessage msg attrs
