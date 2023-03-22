import com.azure.messaging.servicebus.administration._
//import com.azure.messaging.servicebus.ServiceBusClientBuilder
//import com.azure.messaging.servicebus.ServiceBusReceivedMessage
import com.azure.messaging.servicebus._


object ServiceBusClient extends App {


  val connectionString = "Endpoint=sb://wh-bus-develop.servicebus.windows.net/;SharedAccessKeyName=RootManageSharedAccessKey;SharedAccessKey=43XY2o9+11rgrsCrGCqhkyxqIkl4iaN3bWsS5sZ+LTM="
  val adminClient = new ServiceBusAdministrationClientBuilder()
    .connectionString(connectionString)
    .buildClient()

//  val topicIterator = adminClient.listTopics().iterator()
//  while (topicIterator.hasNext()) {
//    val topicProperties = topicIterator.next()
//    println(topicProperties.getName())
//  }
//
  val topicName = "wildhealth.integrationevents.patients/patientintegrationevent"
  val subscriptions = adminClient.listSubscriptions(topicName)
  val subscriptionIterator = subscriptions.iterator()

  while (subscriptionIterator.hasNext()) {
    val subscriptionProperties = subscriptionIterator.next()
    println(subscriptionProperties.getSubscriptionName())
  }

//  https://pmichaels.net/2022/02/19/reading-an-azure-dead-letter-queue-with-azure-messaging-servicebus/

//  val client = new ServiceBusClientBuilder()
//    .connectionString(connectionString)
//    .topicName(topicName)
//    .subscriptionName(subscriptionName)
//    .buildClient()


}
